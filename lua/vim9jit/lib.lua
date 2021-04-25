--- Rewritten version of patterns.lua
--- Could be upstreamed to plenary maybe :)
local lpeg = require('lpeg')

local P, S, C, Ct, Cg, V =
  lpeg.P, lpeg.S, lpeg.C, lpeg.Ct, lpeg.Cg, lpeg.V

local fold = function (func, ...)
  local result = nil
  for _, v in ipairs({...}) do
    if result == nil then
      result = v
    else
      result = func(result, v)
    end
  end
  return result
end

local folder = function (func)
  return function (...)
    return fold(func, ...)
  end
end

local lib = {}

lib.literal = P

--- Takes vargs -> concats all of them and wraps in lpeg.S
lib.set = function(...)
  return lpeg.S(fold(function (a, b) return a .. b end, ...))
end

--- Wrapper around lpeg.R
lib.range = function(t)
  assert(#t == 2, "Must be start and end")
  assert(type(t[1]) == "string", "'start' must be a string")
  assert(type(t[2]) == "string", "'end' must be a string")

  return lpeg.R(t[1] .. t[2])
end

lib.optional = function(t)
  assert(#t == 1, "Must be length 1 table")
  local v = t[1]
  return v ^ -1
end

return lib
