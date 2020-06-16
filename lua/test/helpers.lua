local assert = require('luassert')

-- Default to not printing anything during the tests.
-- We can turn it on or off for specific tests if we want
local token = require('vim9jit.token')
token.print_error = false

local eq = function(exp, act)
  return assert.are.same(exp, act)
end

local neq = function(exp, act)
  return assert.are_not.same(exp, act)
end

local get_first_item = function(t)
  return t[1][1][1]
end

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

return {
  eq=eq,
  neq=neq,
  get_first_item=get_first_item,
  get_item=get_item,
}
