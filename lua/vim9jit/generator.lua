local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local dedent = require('vim9jit.utils').dedent
local indent = require('vim9jit.utils').indent
local trim = vim.trim

local has_inspect, inspect = pcall(require, 'inspect')
if not has_inspect then
  inspect = vim.inspect
end


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

local _ret_value = function(match) return match.value end

local util = require('vim9jit.generator.util')

local get_item = util.get_item
local get_item_with_id = util.get_item_with_id
local id_exists = util.id_exists


local get_result = function(match)
  return util.get_result(generator, match)
end

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
        [[local %s = require('vim9jit').AssertType("%s", %s)%s]],
        identifier,
        type_definition,
        expression,
        "\n"
      )
    end
  end

  local prefix
  if is_global then
    prefix = get_result(get_item_with_id(match, 'GlobalVariableIdentifier'))
  else
    prefix = string.format("%s%s", local_prefix and 'local ' or '', identifier)
  end

  -- This handles things like `let x: number`
  if expression == nil and type_definition then
    expression = string.format([[require('vim9jit').DefaultForType("%s")]], type_definition)
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

generator.match.Var = function(match)
  return _assignment(match, true)
end

generator.match.Expression = function(match)
  local output = ''
  for _, v in ipairs(match) do
    output = output .. get_result(v)
  end

  return output
end

generator.match.ListLiteral = function(match)
  local results = {}
  for _, v in ipairs(match) do
    table.insert(results, get_result(v))
  end

  return string.format(
    "{ %s }",
    table.concat(results, ", ")
  )
end

-- var x = cond ? foo : bawr
--[[


{ a, b, c}

{
  [1] = { ... cond }, <-- number.
  [2] = { ... foo  },
  [3] = { ... bawr },

  ["id"] = "cond ? foo : bawr"
  ["value"] = asdf
}

--]]
generator.match.ConditionalExpression = function(match)
  return string.format(
    "vim9jit.conditional(%s, function() return %s end, function() return %s end)",
    get_result(match[1]),
    get_result(match[2]),
    get_result(match[3])
  )
end

generator.match.IfBody = generator.match.Expression
generator.match.IfStatement = function(match)
  -- TODO: Rename this to IfExpression
  local if_expression = get_result(match[1])
  local if_body = get_result(get_item_with_id(match, 'IfBody'))

  local else_statement = get_result(get_item_with_id(match, 'ElseStatement'))
  if else_statement then
    else_statement = "\n" .. else_statement
  end

  return string.format(fmt(
    [[
if %s then
%s%s
end
    ]]),
    if_expression, indent(fmt(if_body, false), 2), else_statement
  )
end

generator.match.ElseStatement = function(match)
  local if_body = get_result(get_item_with_id(match, 'IfBody'))

  return string.format("else\n%s", indent(fmt(if_body, false), 2))
end

-- generator.match.ReturnValue = function(match)
-- end
generator.match.ReturnValue = generator.match.Expression
generator.match.Return = function(match)
  return string.format("return %s", get_result(match[1]))
end

generator.match.LambdaLiteral = function(match)
  return string.format(
    "function(%s) return %s end",
    get_result(get_item_with_id(match, "FuncArgList")) or '',
    get_result(get_item_with_id(match, "Expression"))
  )
end

generator.match.FuncName = function(match)
  local original_func_name = match.value

  if original_func_name == "function" then
    return "vim9jit.vim_function"
  elseif string.match(string.sub(original_func_name, 1, 1), "%l") then
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

-- TODO: This may need to handle type things and such...?
generator.match.FuncArg = generator.match.Expression
generator.match.FuncArgList = function(match)
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

local in_place = { ['vim.fn["add"]'] = true }

generator.match.MethodCall = function(match)
  return require('vim9jit.generator.method_call')(match)
end

generator.match.For = function(match)
  local for_var = get_result(get_item_with_id(match, 'ForVar'))
  local for_body = get_result(get_item_with_id(match, 'ForBody'))
  local formatted_for_body = indent(trim(for_body), 2)

  local for_obj_match = get_item_with_id(match, 'ForObj')
  local for_obj_func_name_match = get_item_with_id(for_obj_match, 'FuncName', true) or {}

  if for_obj_func_name_match.value == 'range' then
    local func_call_args = get_item_with_id(for_obj_match, 'FuncCallArgList', true)

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
  return string.format("require('vim9jit').tbl.iter(%s)", get_result(match[1]))
end

generator.match.CommandName = _ret_value
generator.match.CommandBang = function(match) return '"!"' end
generator.match.CommandArguments = generator.match.Expression
generator.match.Command = function(match)
  local command_name = get_result(get_item_with_id(match, 'CommandName'))
  local command_bang = get_result(get_item_with_id(match, 'CommandBang')) or '""'
  local command_arguments = get_result(get_item_with_id(match, 'CommandArguments'))

  if command_arguments then
    return string.format(
      "vim.cmd(string.format(%s, '%s', %s, %s))\n",
      "[[%s%s '%s']]",
      command_name,
      command_bang,
      command_arguments
    )
  else
    return string.format(
      "vim.cmd(string.format(%s, '%s', %s))\n",
      "[[%s%s]]",
      command_name,
      command_bang
    )
  end
end

generator.match.UnparsedCapturedError = function(match)
  return inspect(match)
end

generator.match.Boolean = function(match)
  local value = match.value

  if string.find(value, "true") then
    return "true"
  else
    return "false"
  end
end

generator.match.ComparisonExpression = function(match)
  -- Operator, value, value
  return string.format(
    "require('vim9jit').ComparisonEvaluate([[%s]], %s, %s)",
    get_result(match[2]),
    get_result(match[1]),
    get_result(match[3])
  )
end

generator.match.BinaryExpression = function(match)
  local operator = get_result(match[2])
  local format_str = require('vim9jit.generator.binary_expression')[operator]
  if not format_str then
    format_str = "require('vim9jit').BinaryExpression([[" .. operator ..  "]], %s, %s)"
  end

  return string.format(
    format_str,
    get_result(match[1]),
    get_result(match[3])
  )
end

generator.match.BinaryOperator = _ret_value

generator.match.Comment = function(match)
  return string.format("-- %s", match.value)
end

generator.match.VariableIdentifier = _ret_value

local _dict_value = function(accessor)
  return function(match)
    return string.format("%s['%s']", accessor, get_result(match[1]))
  end
end
generator.match.GlobalVariableIdentifier = _dict_value('vim.g')
generator.match.VimVariableIdentifier = _dict_value('vim.v')

generator.match.FuncBody = generator.match.Expression
generator.match.ForBody = generator.match.Expression

generator.match.FuncStatement = function(match)
  -- TODO: 
  --    - There are names of functions in vimscript
  --        that are NOT valid identifiers to be used directly in Lua.
  --        We probably need to do something like
  --        _ENV["func_name"] = function() ... end
  --    - Probably want to expose these as s:local functions for vimscript land
  --        so that people can use the script hax they are used to.
  return string.format([[local function %s(%s) %s end]],
    get_result(get_item_with_id(match, "FuncName")),
    get_result(get_item_with_id(match, "FuncArgList")) or '',
    get_result(get_item_with_id(match, "FuncBody"))
  )
end

generator.match.StringLiteral = _ret_value
generator.match.ForVar = _ret_value
generator.match.TypeDefinition = _ret_value
generator.match.StringOperator = _ret_value
generator.match.Number = _ret_value
generator.match.CapturedEOL = _ret_value
generator.match.ComparisonExpressionOperator  = _ret_value

generator._utils = {}
generator._utils.fmt = fmt
generator._utils.set_grammar = function(g)
  grammar = g
end

return generator
