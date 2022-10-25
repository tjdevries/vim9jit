local to_vim_bool = require("vim9script.convert").to_vim_bool

local ops = {}

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

ops["NotEqualTo"] = function(left, right)
  return not ops["EqualTo"](left, right)
end

ops["LessThan"] = function(left, right)
  return left < right
end

ops["LessThanOrEqual"] = function(left, right)
  return left <= right
end

ops["GreaterThan"] = function(left, right)
  return left > right
end

ops["GreaterThanOrEqual"] = function(left, right)
  return left >= right
end

ops["RegexpMatches"] = function(left, right)
  return not not vim.regex(right):match_str(left)
end

ops["RegexpMatchesIns"] = function(left, right)
  return not not vim.regex("\\c" .. right):match_str(left)
end

ops["NotRegexpMatches"] = function(left, right)
  return not ops["RegexpMatches"](left, right)
end

ops["Modulo"] = function(left, right)
  return left % right
end

ops["Minus"] = function(left, right)
  -- TODO: This is not right :)
  return left - right
end

return ops
