local tree = {}

tree.get_item = function(t, param, key, recursive, result_number, current_found)
  if t == nil then
    return nil
  end

  if recursive == nil then
    recursive = false
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

  if not recursive then
    return nil
  end

  local result = nil
  for _, v in ipairs(t) do
    if type(v) == "table" then
      result = tree.get_item(v, param, key, result_number, current_found)
      if result then
        current_found = current_found + 1
        if current_found >= result_number then
          return result
        end
      end
    end
  end

  return result
end

tree.get_item_with_id = function(match, id, recursive)
  return tree.get_item(match, "id", id, recursive)
end

return tree
