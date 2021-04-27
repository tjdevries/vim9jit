local p = require('vim9jit.patterns')
local g = require('vim9jit.grammar')

local lib = require('vim9jit.lib')

local range = lib.range
local _whitespace_table = {
  ' ',
  '\t',
  '\v',
  '\f'
}
local whitespace = p.set(unpack(_whitespace_table))

local grammar_unpack = function(t)
  local results = {}

  local spaces = p.any_amount(whitespace)

  if t.linespace then
    table.insert(results, spaces)
  end

  for _, v in ipairs(t) do
    if type(v) == "string" then
      table.insert(results, p.literal(v))
    else
      table.insert(results, v)
    end

    if t.linespace then
      table.insert(results, spaces)
    end
  end

  return unpack(results)
end

local literal = lib.literal
local any_amount = function(t)
  assert(#t == 1)
  if t.capture == false then
    return p.any_amount(t[1])
  else
    return p.capture(p.any_amount(t[1]))
  end
end

local any_whitespace = any_amount { whitespace }

local any_amount_of = function(t)
  return p.any_amount(p.branch(grammar_unpack(t)))
end

local _wrap = function(generator)
  return function(t)
    local res = generator(grammar_unpack(t))
    if t.capture then res = p.capture(res) end
    return res
  end
end

local seq = _wrap(p.concat)
local set = _wrap(p.branch)
local some = _wrap(function(...)
  local args = {...}
  assert(#args == 1, "Must be one: some")
  return p.one_or_more(args[1])
end)
local one_or_no = _wrap(function(...)
  local args = {...}
  assert(#args == 1, "Must be one: one_or_no")
  return p.one_or_no(args[1])
end)

local list_of = function(t)
  local patt = seq(t)
  local pow = t.required and 1 or 0

  -- could change it into the new method
  return patt * (t.separator * patt)^pow
end
-- local list_of = _wrap(function(...)
--   local t = seq({...})

--   assert(t.separator, "Must pass a separator")

--   local pow = t.required and 1 or 0

--   -- could change it into the new method
--   return t[1] * (t.separator * t[1])^pow
-- end)

-- local optional = _wrap(lib.optional)
local optional = function(t)
  local result = lib.optional(t)
  if t.capture then
    result = p.capture(result)
  end
  return result
end

local underscore = p.literal("_")
local digit = p.range('0', '9')
local letter = p.branch(
  p.range('a', 'z'),
  p.range('A', 'Z')
)

local whitespace_and_eol = p.set('\n', unpack(_whitespace_table))

local some_whitespace = p.one_or_more(whitespace)
local any_whitespace = p.any_amount(whitespace)
local any_whitespace_or_eol = p.any_amount(whitespace_and_eol)

p.capture_seq_of_pat_with_optional_trailing_sep = function(pat, sep)
  return p.concat(
    any_whitespace_or_eol,
    p.one_or_no(p.concat(
      pat,
      p.any_amount(p.concat(
        any_whitespace_or_eol,
        sep,
        any_whitespace_or_eol,
        pat
      )),
      p.one_or_no(sep)
    )),
    any_whitespace_or_eol
  )
end

local c_seqw = function(sequence)
  local spaces = any_whitespace
  if sequence.eol then
    spaces = any_whitespace_or_eol
  end

  local sequence_with_whitespace = {spaces}
  for i, v in ipairs(sequence) do
    table.insert(sequence_with_whitespace, v)

    -- Don't allow newlines until we say so explicitly
    if i == #sequence then
      table.insert(sequence_with_whitespace, any_whitespace)
    else
      table.insert(sequence_with_whitespace, spaces)
    end
  end
  return p.capture(p.concat(unpack(sequence_with_whitespace)))
end

local EOL = p.end_of_line
local EOF = p.end_of_file
local EOL_or_EOF = p.branch(EOL, p.end_of_file)

local group = setmetatable({}, {
  __index = function(_, k)
    return p.group(k)
  end,
})

local make_grammar = function(root)
  local result = {
    root or "vim9script",

    vim9script = seq {
      "vim9script", EOL_or_EOF,
      any_amount {
        set {
          seq { group.Assign, EOL_or_EOF },
          -- EOF,
        },
      },

      capture = true,
    },

    Assign = c_seqw {
      group.VarName, "=", group.Expression,

      eol = false,
    },

    VarName = seq {
      set {
        letter, underscore
      },
      any_amount_of {
        letter, underscore, digit
      },

      capture = true,
    },

    --
    -- TODO:
    -- - exponentiation

    --[[
      exp = lpeg.V("term") + lpeg.V("factor") + integer,
      term = node((lpeg.V("factor") + integer) * addsub * lpeg.V("exp")),
      factor = node(integer * muldiv * (lpeg.V("factor") + integer))
    --]]

    Expression = seq {
      any_whitespace, 
      set {
        group.Term,
        group.Factor,

        group.AnchoredExpression,

        capture = true,
      },
    },

    -- The singular items that can be used anywhere as one "unit"
    AnchoredExpression = set {
      group.FuncCall,
      group.ObjectBracketAccess,
      group.DictionaryLiteral,
      group.ListLiteral,
      group.StringLiteral,

      group.Number,
      group.Boolean,
      group.VarName,

      group.ParenthedExpression,
    },

    ParenthedExpression = seq {
      any_whitespace, "(", any_whitespace, group.Expression, any_whitespace, ")", any_whitespace,
    },

    Add = literal "+",
    Subtract = literal "-",

    Multiply = literal "*",
    Divide = literal "/",

    Term = seq {
      set { group.Factor, group.AnchoredExpression, },
      set { group.Add, group.Subtract },
      group.Expression,

      capture = true,
      linespace = true,
    },

    Factor = seq {
      group.AnchoredExpression,
      set { group.Multiply, group.Divide, },
      set { group.Factor, group.AnchoredExpression, },

      capture = true,
      linespace = true,
    },

    Unary = set { "+", "-" },

    Number = seq {
      optional { group.Unary, },
      set {
        -- Hexadecimal
        seq {
          set { "0x", "0X", },
          some {
            set { digit, range { 'a', 'f' }, range { 'A', 'F' } }
          }
        },

        -- Float
        seq {
          some { digit }, '.', some { digit }
        },

        some { digit },
      },

      capture = true,
    },

    Boolean = set {
      "true", "v:true", "false", "v:false", capture = true,
    },

    StringLiteral = set {
      --  Courtesy of Lua wiki. Thanks!
      p.literal("'") * ((1 - p.S"'\r\n\f\\") + (p.literal '\\' * 1)) ^ 0 * "'",
      p.literal('"') * ((1 - p.S'"\r\n\f\\') + (p.literal '\\' * 1)) ^ 0 * '"',

      capture = true,
    },

    ObjectBracketAccess = seq {
      set { group.ListLiteral, group.DictionaryLiteral },
      '[', group.Expression, ']',

      linespace = true,
    },

    ListLiteral = seq {
      '[',
      p.capture_seq_of_pat_with_optional_trailing_sep(group.Expression, literal ","),
      ']',

      linespace = true,
    },

    DictionaryKey = set {
      group.VarName,
    },

    DictionaryValue = set {
      group.AnchoredExpression,
    },

    DictionaryItem = seq {
      group.DictionaryKey,
      ':',
      group.DictionaryValue,

      linespace = true,
      capture = true,
    },

    DictionaryLiteral = seq {
      '{',
        list_of {
          group.DictionaryItem,

          separator = ',',
          linespace = true,
        },
      '}',
    },

    -- {{{ Function Calls
    FuncCall = seq {
      group.FuncName, "(", group.FuncCallArgList, ")",

      linespace = true,
    },

    FuncName = set {
      group.VarName,

      capture = true,
    },

    FuncCallArg = group.Expression,

    FuncCallArgList = one_or_no {
      list_of {
        group.FuncCallArg,

        separator = ",",
        linespace = true,
      }
    },
    -- }}}
  }

  return g.grammar(result)
end

return {
  make_grammar = make_grammar
}
