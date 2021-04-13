local p = require('vim9jit.patterns')
local g = require('vim9jit.grammar')

local literal = p.literal
local group = p.group
local branch = function(t)
  return p.branch(unpack(t))
end

local any_amount = function(t)
  assert(#t == 1)
  if t.capture == false then
    return p.any_amount(t[1])
  else
    return p.capture(p.any_amount(t[1]))
  end
end

local any_amount_of = function(t)
  return p.any_amount(p.branch(unpack(t)))
end

local c_seq = function(t)
  return p.capture(p.concat(unpack(t)))
end

local seq = function(t)
  return p.concat(unpack(t))
end

local underscore = p.literal("_")
local digit = p.range('0', '9')
local letter = p.branch(
  p.range('a', 'z'),
  p.range('A', 'Z')
)

local _whitespace_table = {
  ' ',
  '\t',
  '\v',
  '\f'
}

local whitespace = p.set(unpack(_whitespace_table))
local whitespace_and_eol = p.set('\n', unpack(_whitespace_table))

local some_whitespace = p.one_or_more(whitespace)
local any_whitespace = p.any_amount(whitespace)
local any_whitespace_or_eol = p.any_amount(whitespace_and_eol)

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
local EOL_or_EOF = p.branch(EOL, p.end_of_file)


local make_grammar = function(root)
  return g.grammar {
    root or "vim9script",

    vim9script = c_seq {
      literal "vim9script", EOL_or_EOF,
      any_amount {
        branch {
          seq { group "Assign", EOL_or_EOF },
          EOF,
        },

        capture = false,
      },
    },

    Assign = c_seqw {
      group   "VarName",
      literal "=",
      group   "Expression",

      eol = true,
    },

    VarName = c_seq {
      branch { 
        letter, underscore
      },
      any_amount_of {
        letter, underscore, digit
      }
    },

    Expression = c_seq {
      group "VarName",
    },
  }
end

return {
  make_grammar = make_grammar
}
