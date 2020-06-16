local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local inspect = require('inspect') or vim.inspect

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
    output = output .. generator.match[v.id](v)
  end

  return output
end

generator.match = {}

generator.match.Let = function(match, ...)
  local is_global = id_exists(match, 'GlobalVariableIdentifier')

  local identifier = get_result(get_item_with_id(match, 'VariableIdentifier'))
  local expression = get_result(get_item_with_id(match, 'Expression'))

  if STRICT then
    local type_definition = get_item_with_id(match, 'TypeDefinition')

    if type_definition then
      return string.format(
        [[local %s = vim9jit.AssertType("%s", %s)]],
        identifier,
        get_result(type_definition),
        expression
      )
    end
  end

  local prefix
  if is_global then
    prefix = string.format('vim.g[\"%s\"]', identifier)
  else
    prefix = string.format("local %s", identifier)
  end

  return string.format(
    [[%s = %s]],
    prefix,
    expression
  )
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
