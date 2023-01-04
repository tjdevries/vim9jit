local lib = require('_vim9script')

local M = {}

M['And'] = function(left, right)
  return lib.bool(left) and lib.bool(right)
end

M['Or'] = function(left, right)
  return lib.bool(left) or lib.bool(right)
end

M['Plus'] = function(left, right)
  return left + right
end

M['Multiply'] = function(left, right)
  return left * right
end

M['Divide'] = function(left, right)
  return left / right
end

M['StringConcat'] = function(left, right)
  return left .. right
end

M['EqualTo'] = function(left, right)
  return left == right
end

M['NotEqualTo'] = function(left, right)
  return not M['EqualTo'](left, right)
end

M['LessThan'] = function(left, right)
  return left < right
end

M['LessThanOrEqual'] = function(left, right)
  return left <= right
end

M['GreaterThan'] = function(left, right)
  return left > right
end

M['GreaterThanOrEqual'] = function(left, right)
  return left >= right
end

M['RegexpMatches'] = function(left, right)
  return not not vim.regex(right):match_str(left)
end

M['RegexpMatchesIns'] = function(left, right)
  return not not vim.regex('\\c' .. right):match_str(left)
end

M['NotRegexpMatches'] = function(left, right)
  return not M['RegexpMatches'](left, right)
end

M['Modulo'] = function(left, right)
  return left % right
end

M['Minus'] = function(left, right)
  -- TODO: This is not right :)
  return left - right
end

return M
