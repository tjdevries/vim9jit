
local get_item

get_item = function(t, param, key, recursive, result_number, current_found)
  if t == nil then
    return nil
  end

  recursive = recursive == nil and true or false

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

  if not recursive then
    return nil
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
  return get_item(match, 'id', id, false) ~= nil
end

local get_result = function(generator, match)
  if match == nil then
    return nil
  end

  local match_id = match.id
  assert(match_id, string.format("%s:%s malformed object", match_id, match))

  -- Hmm... wonder if I could just use _ret_value for a lot of stuff.
  assert(generator.match[match_id], string.format("%s: missing generator", match_id))

  return generator.match[match_id](match)
end

return {
  get_item = get_item,
  get_item_with_id = get_item_with_id,
  id_exists = id_exists,
  get_result = get_result,
}
