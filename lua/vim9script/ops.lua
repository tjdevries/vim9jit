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
  elseif val == nil then
    return false
  end

  error("unhandled type: " .. vim.inspect(val))
end

ops["And"] = function(left, right)
  return to_vim_bool(left) and to_vim_bool(right)
end

ops["Or"] = function(left, right)
  return to_vim_bool(left) or to_vim_bool(right)
end

ops["Plus"] = function(left, right)
  return left + right
end

ops["StringConcat"] = function(left, right)
  return left .. right
end

ops["EqualTo"] = function(left, right)
  return left == right
end

ops["RegexpMatches"] = function(left, right)
  return vim.regex(right):match_str(left)
end

ops["Modulo"] = function(left, right)
  return left % right
end

return ops
