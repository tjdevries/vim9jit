local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local inspect = require('inspect') or vim.inspect

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
  -- print(inspect(parsed))

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

generator.match.TypeDefinition = _ret_value
generator.match.AdditionOperator = _ret_value
generator.match.VariableIdentifier = _ret_value
generator.match.Number = _ret_value

return generator
