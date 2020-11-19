-- Taken from one of my other pluins: mparse.nvim
-- Might want to make some of this easier or less complicated.
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

local patterns = {}

patterns.match = lpeg.match
patterns.V = function(item) return lpeg.V(item) end
patterns.table_capture = function(item) return lpeg.Ct(item) end
patterns.group_capture = Cg
patterns.back_capture = lpeg.Cb
patterns.function_capture = lpeg.Cmt

-- Determines if the start, finish are closed
-- and creates a group capture of "name"
patterns.closed = function(start, finish, name)
  return P({
    start * Cg(((1 - S(start .. finish)) + V(1))^0, name) * finish
  })
end

patterns.list_of = function(patt, sep, required)
  patt, sep = P(patt), P(sep)

  local pow = required and 1 or 0

  return patt * (sep * patt)^pow
end

patterns.g_paren = patterns.closed('(', ')', '_')

patterns.split = function(value, sep)
  local g_split = P({
    Ct(V("elem") * (V("sep") * V("elem"))^0),

    sep = S(sep .. "()"),
    elem = C(((1-V("sep")) + patterns.g_paren)^0),
  })

  return lpeg.match(g_split, value)
end


patterns.S = S
patterns.literal = lpeg.P
patterns.set = function(...)
  return lpeg.S(fold(function (a, b) return a .. b end, ...))
end
patterns.range = function(s, e) return lpeg.R(s .. e) end
patterns.concat = folder(function (a, b) return a * b end)
patterns.branch = folder(function (a, b) return a + b end)
patterns.one_or_more = function(v) return v ^ 1 end
patterns.two_or_more = function(v) return v ^ 2 end
patterns.any_amount = function(v) return v ^ 0 end
patterns.one_or_no = function(v) return v ^ -1 end
patterns.look_behind = lpeg.B
patterns.look_ahead = function(v) return #v end
patterns.neg_look_ahead = function(v) return -v end
patterns.neg_look_behind = function(v) return -patterns.look_behind(v) end
patterns.optional_surrounding = function(start, finish, v)
  return patterns.branch(
    patterns.concat(
      start,
      v,
      finish
    ),
    v
  )
end
patterns.optional_surrounding_parenths = function(v)
  return patterns.optional_surrounding(
    patterns.literal('('),
    patterns.literal(')'),
    v
  )
end

patterns.capture = function(a)
  return C(a)
end

patterns.zero_match = lpeg.P(0)
patterns.any_character = lpeg.P(1)
patterns.end_of_file = -lpeg.P(1)
patterns.end_of_line = patterns.branch(
  patterns.literal("\r\n"),
  patterns.literal("\r"),
  patterns.literal("\n")
)
patterns.start_of_line = patterns.branch(
  patterns.look_behind(patterns.literal("\n")),
  -- patterns.look_behind(patterns.literal("\r")),
  patterns.look_behind(patterns.literal(''))
)

patterns.subset_expression = function(tokens, operators)
  return patterns.concat(
    patterns.one_or_no(
      operators
    ),
    tokens,
    patterns.any_amount(
      patterns.branch(
        operators
        , tokens
      )
    )
  )
end

patterns.up_to = function(what)
  return ((P(1) - P(what))^1) * P(what)
end

-- luacheck: ignore 142
function string:split(s)
  if self == nil then
    return {}
  end

  local t = {}
  local final_pattern = "(.-)" .. s
  local last_end = 1
  local s, e, cap = self:find(final_pattern, 1)
  while s do
    if s ~= 1 or cap ~= "" then
      table.insert(t, cap)
    end

    last_end = e + 1
    s, e, cap = self:find(final_pattern, last_end)
  end

  if last_end <= #self then
    cap = self:sub(last_end)
    table.insert(t, cap)
  end

  return t
end

return patterns
