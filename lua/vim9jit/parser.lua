-- local l = require('lpeg')

package.loaded['vim9jit.patterns'] = nil
package.loaded['vim9jit.token'] = nil

local patterns = require('vim9jit.patterns')
local token = require('vim9jit.token')

local V = patterns.V

local whitespace = patterns.set(
  ' ',
  '\t',
  '\v',
  '\f'
)
local EOL = patterns.branch(
  patterns.end_of_line,
  patterns.end_of_file
)

local digit = patterns.range('0', '9')
local letter = patterns.branch(
  patterns.range('a', 'z'),
  patterns.range('A', 'Z')
)
local underscore = patterns.literal("_")

local _addition_operator = patterns.capture(patterns.literal("+"))

local identifier = patterns.one_or_more(patterns.branch(
  letter,
  underscore
))

local grammar = token.define(function(_ENV)
  START "vim9script"

  SUPPRESS(
    "ArithmeticTokens"
    , "ArithmeticExpression"

    , "_VarName"
  )

  vim9script = patterns.capture(patterns.concat(
    patterns.literal("vim9script"), EOL,
    patterns.any_amount(
      patterns.concat(
        patterns.branch(
          V("Let"),
          V("Assign")
        ),
        patterns.one_or_no(EOL)
      )
    )
  ))

  Number = patterns.capture(patterns.one_or_more(digit))

  VariableIdentifier = patterns.capture(identifier)
  GlobalVariableIdentifier = patterns.capture(patterns.concat(
    patterns.literal("g:"),
    V("VariableIdentifier")
  ))
  VimVariableIdentifier = patterns.capture(patterns.concat(
    patterns.literal("v:"),
    V("VariableIdentifier")
  ))

  PrimitivesDictIdentifier = patterns.capture(patterns.branch(
    patterns.literal("b"),
    patterns.literal("t"),
    patterns.literal("w")
  ))
  PrimitivesVariableIdentifier = patterns.capture(patterns.concat(
    V("PrimitivesDictIdentifier"),
    patterns.literal(":"),
    V("VariableIdentifier")
  ))

  AdditionOperator = patterns.capture(patterns.concat(
    patterns.any_amount(whitespace),
    _addition_operator,
    patterns.any_amount(whitespace)
  ))

  ArithmeticTokens = patterns.optional_surrounding_parenths(
    patterns.branch(
      V("Number"),
      V("VariableIdentifier")
    )
  )

  ArithmeticExpression = patterns.concat(
    V("ArithmeticTokens"),
    patterns.one_or_more(
      patterns.concat(
        V("AdditionOperator"),
        V("ArithmeticTokens")
      )
    )
  )

  Expression = patterns.branch(
    V("ArithmeticExpression"),
    V("Number")
  )

  TypeDefinition = patterns.concat(
    patterns.literal(":"),
    patterns.any_amount(whitespace),
    patterns.capture(patterns.any_amount(letter))
  )

  _VarName = patterns.branch(
    V("GlobalVariableIdentifier"),
    V("VimVariableIdentifier"),
    V("PrimitivesVariableIdentifier"),
    V("VariableIdentifier")
  )

  Let = patterns.capture(patterns.concat(
    patterns.any_amount(whitespace),
    patterns.literal("let"),
    patterns.one_or_more(whitespace),
    V("_VarName"),
    patterns.one_or_no(V("TypeDefinition")),
    patterns.one_or_no(patterns.concat(
      patterns.one_or_more(whitespace),
      patterns.literal("="),
      patterns.any_amount(whitespace),
      V("Expression")
    )),
    EOL
  ))

  Assign = patterns.capture(patterns.concat(
    -- TODO: Maybe could put this whitespace into the resulting lua so it isn't so ugly...
    patterns.any_amount(whitespace),
    V("_VarName"),
    patterns.one_or_more(whitespace),
    patterns.literal("="),
    patterns.any_amount(whitespace),
    V("Expression"),
    EOL
  ))

  Set = patterns.capture(patterns.concat(
    patterns.any_amount(whitespace)
    , patterns.literal("set")
    , patterns.any_amount(whitespace)
  ))

end)

return {
  grammar = grammar
}
