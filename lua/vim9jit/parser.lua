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

local some_whitespace = patterns.one_or_more(whitespace)
local any_whitespace = patterns.any_amount(whitespace)

local EOL = patterns.end_of_line
local EOL_or_EOF = patterns.branch(EOL, patterns.end_of_file)

local digit = patterns.range('0', '9')
local letter = patterns.branch(
  patterns.range('a', 'z'),
  patterns.range('A', 'Z')
)

local comma = patterns.literal(",")
local underscore = patterns.literal("_")
local left_paren = patterns.literal("(")
local right_paren = patterns.literal(")")


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
    , "ValidLine"

    , "_VarName"
  )

  vim9script = patterns.capture(patterns.concat(
    patterns.literal("vim9script"), EOL_or_EOF,
    patterns.any_amount(
      patterns.concat(
        patterns.branch(
          V("FuncDef")
          , V("For")
          , V("Let")
          , V("Assign")
          , V("Command")
          , patterns.concat(V("FuncCall"), V("CapturedEOL"))
          , patterns.concat(patterns.any_amount(whitespace), patterns.end_of_line)
        )
      )
    ),
    patterns.branch(
      patterns.one_or_no(patterns.end_of_file)
      -- , V("UnparsedCapturedError")
    )
  ))

  CapturedEOL = patterns.capture(EOL)

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
    any_whitespace,
    _addition_operator,
    any_whitespace
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

  FuncCallArg = patterns.capture(
    patterns.branch(
      V("Expression")
      , V("VariableIdentifier")
    )
  )
  FuncCallArgList = patterns.capture(
    patterns.one_or_no(patterns.concat(
      V("FuncCallArg"),
      patterns.any_amount(patterns.concat(
        any_whitespace, comma, any_whitespace
        , V("FuncCallArg")
      ))
    ))
  )

  FuncCall = patterns.capture(patterns.concat(
    V("FuncName"),
    left_paren,
    any_whitespace,
    V("FuncCallArgList"),
    any_whitespace,
    right_paren
  ))

  Expression = patterns.branch(
    V("ArithmeticExpression")
    , V("Number")
    , V("FuncCall")
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
    EOL_or_EOF
  ))

  Assign = patterns.capture(patterns.concat(
    -- TODO: Maybe could put this whitespace into the resulting lua so it isn't so ugly...
    patterns.any_amount(whitespace),
    V("_VarName"),
    patterns.one_or_more(whitespace),
    patterns.literal("="),
    patterns.any_amount(whitespace),
    V("Expression"),
    EOL_or_EOF
  ))

  Set = patterns.capture(patterns.concat(
    patterns.any_amount(whitespace)
    , patterns.literal("set")
    , patterns.any_amount(whitespace)
  ))

  -- TODO: This needs more options
  ForVar = patterns.capture(
    V("_VarName")
  )

  ForObj = patterns.capture(patterns.branch(
    V("FuncCall")
    , V("_VarName")
  ))

  ForBody = patterns.capture(patterns.any_amount(V("ValidLine")))

  For = patterns.capture(patterns.concat(
    patterns.any_amount(whitespace)
    , patterns.literal("for")
        , some_whitespace
        , V("ForVar")
        , some_whitespace
        , patterns.literal("in")
        , some_whitespace
        , V("ForObj"), any_whitespace, EOL
      , V("ForBody")
    , any_whitespace, patterns.literal("endfor")
  ))

  ReturnValue = patterns.capture(V("Expression"))
  Return = patterns.capture(patterns.concat(
    any_whitespace,
    patterns.literal("return"),
    any_whitespace,
    V("ReturnValue")
  ))

  ValidLine = patterns.branch(
    V("FuncDef")
    , V("Return")
    , V("For")
    , V("Let")
    , V("Assign")
    , V("Set")
    , V("Command")
    -- TODO: Not exactly true...
    , patterns.end_of_line
  )

  FuncArg = patterns.capture(
    V("_VarName")
  )

  FuncArgList = patterns.capture(patterns.concat(
    V("FuncArg"),
    patterns.any_amount(patterns.concat(
      patterns.any_amount(whitespace),
      comma,
      patterns.any_amount(whitespace),
      V("FuncArg")
    ))
  ))

  FuncBody = patterns.capture(patterns.branch(
    patterns.one_or_more(V("ValidLine"))
    -- , patterns.any_amount(V("UnparsedCapturedError"))
  ))

  FuncName = patterns.capture(V("_VarName"))

  FuncDef = patterns.capture(patterns.concat(
    patterns.any_amount(whitespace),
    patterns.literal("def"),
    patterns.one_or_more(whitespace),
    V("FuncName"),
    left_paren,
    patterns.one_or_no(V("FuncArgList")),
    right_paren,
    patterns.one_or_no(V("TypeDefinition")),
    any_whitespace, EOL,
    patterns.one_or_no(V("FuncBody")),
    any_whitespace,
    patterns.literal("enddef"), EOL
  ))

  CommandName = patterns.capture(V("_VarName"))
  CommandArguments = patterns.capture(V("Expression"))

  Command = patterns.capture(patterns.concat(
    any_whitespace,
    V("CommandName"),
    some_whitespace,
    V("CommandArguments")
  ))


  UnparsedError = (1 - EOL) ^ 1
  UnparsedCapturedError = patterns.capture(V("UnparsedError"))
end)

return {
  grammar = grammar
}
