local ops = {}

local to_vim_bool = function(val)
  if type(val) == "boolean" then
    return val
  elseif type(val) == "number" then
    return val ~= 0
  elseif type(val) == "string" then
    return string.len(val) ~= 0
  elseif type(val) == "table" then
    return not vim.tbl_isempty(val)
  end

  error "unhandled type"
end

ops.AND = function(left, right)
  return to_vim_bool(left) and to_vim_bool(right)
end

ops.OR = function(left, right)
  return to_vim_bool(left) or to_vim_bool(right)
end

return ops
