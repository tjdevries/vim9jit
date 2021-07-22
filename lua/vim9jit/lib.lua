--- Rewritten version of patterns.lua
--- Could be upstreamed to plenary maybe :)
local lpeg = require('lpeg')

local lib = {}

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

local capture = C
local vararg_set = function(...)
  return lpeg.S(fold(function (a, b) return a .. b end, ...))
end

local _whitespace_table = {
  ' ',
  '\t',
  '\v',
  '\f'
}

lib.literal = P
lib.group = V

lib.whitespace = vararg_set(unpack(_whitespace_table))
lib.whitespace_and_eol = vararg_set('\n', unpack(_whitespace_table))

local any_amount = function(patt)
  return patt ^ 0
end

lib.grammar_unpack = function(t)
  local results = {}

  local spaces = any_amount(lib.whitespace)

  if t.linespace then
    table.insert(results, spaces)
  end

  for _, v in ipairs(t) do
    if type(v) == "string" then
      table.insert(results, lib.literal(v))
    else
      table.insert(results, v)
    end

    if t.linespace then
      table.insert(results, spaces)
    end
  end

  return unpack(results)
end


--- Takes vargs -> concats all of them and wraps in lpeg.S
lib.set = function(...)
  return lpeg.S(fold(function (a, b) return a .. b end, ...))
end

local _wrap = function(generator)
  return function(t)
    local res = generator(lib.grammar_unpack(t))
    if t.capture then res = capture(res) end
    return res
  end
end

lib.seq = _wrap(folder(function (a, b) return a * b end))
lib.set = _wrap(folder(function (a, b) return a + b end))
lib.some = _wrap(function(...)
  local args = {...}
  assert(#args == 1, "Must be one: some")
  return args[1] ^ 1
end)

lib.any_amount = function(t)
  assert(#t == 1)
  local patt = t[1] ^ 0
  if t.capture then
    patt = capture(patt)
  end

  return patt
end

lib.one_or_no = _wrap(function(...)
  local args = {...}
  assert(#args == 1, "Must be one: one_or_no")
  return args[1] ^ -1
end)

lib.one_or_more = _wrap(function(...)
  local args = {...}
  assert(#args == 1, "Must be one: one_or_more")
  return args[1] ^ 1
end)

lib.list_of = function(t)
  local patt = lib.seq(t)
  local pow = t.required and 1 or 0
  local sep = assert(t.separator, "Must have a separtor for list_of")

  local spaces = lib.any_whitespace_or_eol

  -- could change it into the new method
  local result = patt * (spaces * sep * spaces * patt)^pow
  if t.trailing then
    result = result * spaces * lib.optional(sep) * spaces
  end

  return result
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
  local patt = t[1] ^ -1
  if t.capture then
    patt = capture(patt)
  end

  return patt
end

lib.any_whitespace = lib.any_amount { lib.whitespace }
lib.any_whitespace_or_eol = lib.any_amount { lib.whitespace_and_eol }
lib.letter = lib.set {
  lib.range{ 'a', 'z' },
  lib.range{ 'A', 'Z' },
}

return lib
