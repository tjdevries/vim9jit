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
  )

  vim9script = patterns.capture(patterns.concat(
    patterns.literal("vim9script"), EOL,
    patterns.any_amount(V("Let"))
  ))

  Number = patterns.capture(patterns.one_or_more(digit))

  VariableIdentifier = patterns.capture(identifier)
  GlobalVariableIdentifier = patterns.capture(patterns.concat(
    patterns.literal("g:"),
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

  Let = patterns.capture(patterns.concat(
    patterns.literal("let"),
    patterns.any_amount(whitespace),
    patterns.branch(
      V("GlobalVariableIdentifier"),
      V("VariableIdentifier")
    ),
    patterns.one_or_no(V("TypeDefinition")),
    patterns.any_amount(whitespace),
    patterns.literal("="),
    patterns.any_amount(whitespace),
    V("Expression"),
    EOL
  ))

end)

return {
  grammar = grammar
}
