local inspector = {}

function inspector.is_number(node)
  if node.id == "number" then
    return true
  end
  return false
end

return inspector
