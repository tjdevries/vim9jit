local p = require "vim9jit.patterns"
local g = require "vim9jit.grammar"

local lib = require "vim9jit.lib"

local any_amount = lib.any_amount
local letter = lib.letter
local list_of = lib.list_of
local literal = lib.literal
local one_or_more = lib.one_or_more
local one_or_no = lib.one_or_no
local optional = lib.optional
local range = lib.range
local seq = lib.seq
local set = lib.set
local some = lib.some

-- local whitespace = lib.whitespace

local underscore = literal "_"
local digit = range { "0", "9" }

local any_whitespace = lib.any_whitespace
local any_whitespace_or_eol = lib.any_whitespace_or_eol

local c_seqw = function(sequence)
  local spaces = any_whitespace
  if sequence.eol then
    spaces = any_whitespace_or_eol
  end

  local sequence_with_whitespace = { spaces }
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
      "vim9script",
      EOL_or_EOF,
      any_amount {
        set {
          seq { group.Assign, EOL_or_EOF },
          -- EOF,
        },
      },

      capture = true,
    },

    Assign = c_seqw {
      group.Variable,
      "=",
      group.Expression,

      eol = false,
    },

    -- VarKeyword    = literal "var",
    -- ConstKeyword  = literal "const",
    -- FinalKeyworld = literal "final",

    -- Var = seq {
    --   set { group.VarKeyword, group.ConstKeyword, group.FinalKeyworld },

    --   capture = true,
    -- },

    -- TypeDefiniton = seq {
    --   ":", group.TypeSyntax
    -- },

    -- TypeSyntax = list_of {
    --   group.Type_SingleDefinition,

    --   separator = "|",
    -- },

    -- Type_SingleDefinition = set {
    --   'asdf'
    -- },

    VariableIdentifier = seq {
      set {
        letter,
        underscore,
      },
      any_amount {
        set { letter, underscore, digit },
      },

      capture = true,
    },

    BufferVariableIdentifier = seq { "b:", group.VariableIdentifier, capture = true },
    GlobalVariableIdentifier = seq { "g:", group.VariableIdentifier, capture = true },
    ScriptVariableIdentifier = seq { "s:", group.VariableIdentifier, capture = true },
    TabVaraibleIdentifier = seq { "t:", group.VariableIdentifier, capture = true },
    VimVariableIdentifier = seq { "v:", group.VariableIdentifier, capture = true },
    WindowVariableIdentifier = seq { "w:", group.VariableIdentifier, capture = true },

    Variable = set {
      group.BufferVariableIdentifier,
      group.GlobalVariableIdentifier,
      group.ScriptVariableIdentifier,
      group.TabVaraibleIdentifier,
      group.VimVariableIdentifier,
      group.WindowVariableIdentifier,

      group.VariableIdentifier,
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
      group.ObjectDotAccess,
      group.DictionaryLiteral,
      group.ListLiteral,
      group.StringLiteral,

      group.Number,
      group.Boolean,
      group.Variable,

      group.ParenthedExpression,
    },

    ParenthedExpression = seq {
      any_whitespace,
      "(",
      any_whitespace,
      group.Expression,
      any_whitespace,
      ")",
      any_whitespace,
    },

    Add = literal "+",
    Subtract = literal "-",

    Multiply = literal "*",
    Divide = literal "/",

    Term = seq {
      set { group.Factor, group.AnchoredExpression },
      set { group.Add, group.Subtract },
      group.Expression,

      capture = true,
      linespace = true,
    },

    Factor = seq {
      group.AnchoredExpression,
      set { group.Multiply, group.Divide },
      set { group.Factor, group.AnchoredExpression },

      capture = true,
      linespace = true,
    },

    Unary = set { "+", "-" },

    Number = seq {
      optional { group.Unary },
      set {
        -- Hexadecimal
        seq {
          set { "0x", "0X" },
          some {
            set { digit, range { "a", "f" }, range { "A", "F" } },
          },
        },

        -- Float
        seq {
          some { digit },
          ".",
          some { digit },
        },

        some { digit },
      },

      capture = true,
    },

    Boolean = set {
      "true",
      "v:true",
      "false",
      "v:false",
      capture = true,
    },

    StringLiteral = set {
      --  Courtesy of Lua wiki. Thanks!
      p.literal "'" * ((1 - p.S "'\r\n\f\\") + (p.literal "\\" * 1)) ^ 0 * "'",
      p.literal '"' * ((1 - p.S '"\r\n\f\\') + (p.literal "\\" * 1)) ^ 0 * '"',

      capture = true,
    },

    ObjectBracketAccess = seq {
      set { group.ListLiteral, group.DictionaryLiteral, group.ParenthedExpression },
      "[",
      group.Expression,
      "]",

      linespace = true,
    },

    ObjectDotAccess = seq {
      set { group.DictionaryLiteral, group.ParenthedExpression },
      ".",
      group.VariableIdentifier,

      linespace = true,
    },

    ListLiteral = seq {
      "[",
      list_of { group.Expression, separator = "," },
      "]",

      linespace = true,
    },

    DictionaryKeyExpression = seq {
      "[",
      group.Expression,
      "]",
    },

    DictionaryKey = set {
      group.VariableIdentifier,
      -- TODO: Make the [] expression syntax
      group.DictionaryKeyExpression,
    },

    DictionaryValue = group.Expression,

    DictionaryItem = seq {
      group.DictionaryKey,
      ":",
      group.DictionaryValue,

      linespace = true,
      capture = true,
    },

    DictionaryLiteral = seq {
      "{",
      list_of {
        group.DictionaryItem,

        separator = ",",
        linespace = true,
      },
      "}",
    },

    -- {{{ Function Calls
    FuncCall = seq {
      group.FuncName,
      "(",
      group.FuncCallArgList,
      ")",

      linespace = true,
    },

    FuncName = set {
      group.VariableIdentifier,

      capture = true,
    },

    FuncCallArg = group.Expression,

    FuncCallArgList = one_or_no {
      list_of {
        group.FuncCallArg,

        separator = ",",
        linespace = true,
      },
    },
    -- }}}
  }

  return g.grammar(result)
end

return {
  make_grammar = make_grammar,
}
