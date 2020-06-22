local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local dedent = require('vim9jit.utils').dedent
local indent = require('vim9jit.utils').indent
local trim = vim.trim

local inspect = require('inspect') or vim.inspect


local fmt = function(s, ending_newline)
  if string.sub(s, 1, 1) == "\n" then
    s = string.sub(s, 2)
  end

  local eol_string = ''
  if ending_newline == nil or ending_newline == true then
     eol_string = "\n"
  end


  return trim(dedent(s)) .. eol_string
end

-- TODO: Need to extend this further and make the corresponding vim9jit functions to
--          actually do the type checking. That's for another day though.
--          Currently it seems *possible* to do it given the information we've got when parsing at least.
local STRICT = false

local generator = {}

local get_item

get_item = function(t, param, key, result_number, current_found)
  if t == nil then
    return nil
  end

  if result_number == nil then
    result_number = 1
  end

  if current_found == nil then
    current_found = 0
  end

  if t[param] == key then
    return t
  end

  for k, _ in ipairs(t) do
    if t[k][param] == key then
      current_found = current_found + 1

      if current_found >= result_number then
        return t[k]
      end
    end
  end

  local result = nil
  for _, v in ipairs(t) do
    if type(v) == 'table' then
      result = get_item(v, param, key, result_number, current_found)
      if (result) then
        current_found = current_found + 1
        if current_found >= result_number then
          return result
        end
      end
    end
  end

  return result
end

local get_item_with_id = function(match, id)
  return get_item(match, 'id', id)
end

local id_exists = function(match, id)
  return get_item(match, 'id', id) ~= nil
end

local get_result = function(match)
  if match == nil then
    return nil
  end

  local match_id = match.id
  assert(match_id, string.format("%s:%s malformed object", match_id, match))

  -- Hmm... wonder if I could just use _ret_value for a lot of stuff.
  assert(generator.match[match_id], string.format("%s:%s missing generator", match_id, inspect(match)))

  return generator.match[match_id](match)
end


local _ret_value = function(match) return match.value end


generator.generate = function(str, strict)
  STRICT = strict

  local parsed = token.parsestring(grammar, str)
  if parsed == nil then
    error('Unparsed token: ' .. inspect(str))
  end

  local output = ''
  for _, v in ipairs(parsed) do
    local g =  assert(generator.match[v.id], v.id)
    output = output .. g(v)
  end

  return output
end

generator.match = {}

local _assignment = function(match, local_prefix)
  local is_global = id_exists(match, 'GlobalVariableIdentifier')

  local identifier = get_result(get_item_with_id(match, 'VariableIdentifier'))
  local expression = get_result(get_item_with_id(match, 'Expression'))
  local type_definition = get_result(get_item_with_id(match, 'TypeDefinition'))

  if STRICT and local_prefix then
    if type_definition then
      return string.format(
        [[local %s = vim9jit.AssertType("%s", %s)]],
        identifier,
        type_definition,
        expression
      )
    end
  end

  local prefix
  if is_global then
    prefix = string.format('vim.g[\"%s\"]', identifier)
  else
    prefix = string.format("%s%s", local_prefix and 'local ' or '', identifier)
  end

  -- This handles things like `let x: number`
  if expression == nil and type_definition then
    expression = string.format([[vim9jit.DefaultForType("%s")]], type_definition)
  end

  return string.format(
    [[%s = %s%s]],
    prefix,
    expression,
    "\n"
  )
end
generator.match.Assign = function(match)
  return _assignment(match, false)
end

generator.match.Let = function(match)
  return _assignment(match, true)
end

generator.match.Expression = function(match)
  local output = ''
  for _, v in ipairs(match) do
    output = output .. get_result(v)
  end

  return output
end

-- generator.match.ReturnValue = function(match)
-- end
generator.match.ReturnValue = generator.match.Expression
generator.match.Return = function(match)
  return string.format("return %s", get_result(match[1]))
end

generator.match.FuncDef = function(match)
  local func_name = get_result(get_item_with_id(match, 'FuncName'))
  local func_body = get_result(get_item_with_id(match, 'FuncBody'))

  return string.format(fmt(
    [[
local function %s()
%s
end
    ]]),
    func_name, indent(fmt(func_body, false), 2)
  )
end

generator.match.FuncName = function(match)
  local original_func_name = match.value

  if string.match(string.sub(original_func_name, 1, 1), "%l") then
    -- Lowercase functions are always vim functions
    return string.format("vim.fn['%s']", original_func_name)
  else
    return original_func_name
  end
end

generator.match.FuncCallArg = generator.match.Expression
generator.match.FuncCallArgList = function(match)
  local output = {}
  for _, v in ipairs(match) do
    table.insert(output, get_result(v))
  end

  return table.concat(output, ", ")
end

generator.match.FuncCall = function(match)
  local func_name = get_result(get_item_with_id(match, 'FuncName'))
  local func_args = get_result(get_item_with_id(match, 'FuncCallArgList'))

  return string.format([[%s(%s)]], func_name, func_args)
end

generator.match.For = function(match)
  local for_var = get_result(get_item_with_id(match, 'ForVar'))
  local for_body = get_result(get_item_with_id(match, 'ForBody'))
  local formatted_for_body = indent(trim(for_body), 2)

  local for_obj_match = get_item_with_id(match, 'ForObj')
  local for_obj_func_name_match = get_item_with_id(for_obj_match, 'FuncName') or {}

  if for_obj_func_name_match.value == 'range' then
    local func_call_args = get_item_with_id(for_obj_match, 'FuncCallArgList')

    local range_expr
    local range_max
    local range_step
    if #func_call_args == 1 then
      range_expr = 1
      range_max = get_result(func_call_args[1])
      range_step = 1
    elseif #func_call_args == 2 then
      range_expr = get_result(func_call_args[1])
      range_max = get_result(func_call_args[2])
      range_step = 1
    else
      error("Unsupported range style")
    end


    if range_max == nil then
      range_max = range_expr
      range_expr = 1
    end

    return string.format(fmt(
      [[
for %s = %s, %s, %s do
%s
end
      ]]), for_var, range_expr, range_max, range_step, formatted_for_body
    )
  else
    local for_obj = get_result(for_obj_match)

    return string.format(fmt(
      [[
for _, %s in %s do
%s
end
      ]]), for_var, for_obj, formatted_for_body
    )
  end
end

generator.match.ForObj = function(match)
  assert(#match == 1, "ForObj can only have one object here.")
  return string.format('vim9jit.VimPairs(%s)', get_result(match[1]))
end

generator.match.CommandName = _ret_value
generator.match.CommandArguments = generator.match.Expression
generator.match.Command = function(match)
  local command_name = get_result(get_item_with_id(match, 'CommandName'))
  local command_arguments = get_result(get_item_with_id(match, 'CommandArguments'))

  return string.format(
    "vim.cmd(string.format(%s, '%s', %s))\n",
    "[[%s %s]]",
    command_name,
    command_arguments
  )
end

generator.match.UnparsedCapturedError = function(match)
  return inspect(match)
end


generator.match.FuncBody = generator.match.Expression
generator.match.ForBody = generator.match.Expression

generator.match.ForVar = _ret_value
generator.match.TypeDefinition = _ret_value
generator.match.AdditionOperator = _ret_value
generator.match.VariableIdentifier = _ret_value
generator.match.Number = _ret_value
generator.match.CapturedEOL = _ret_value

generator._utils = {}
generator._utils.fmt = fmt

return generator
