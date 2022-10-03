local fn = {}

fn.insert = function(list, item, idx)
  if idx == nil then
    idx = 1
  end

  table.insert(list, idx + 1, item)

  return list
end

return fn
