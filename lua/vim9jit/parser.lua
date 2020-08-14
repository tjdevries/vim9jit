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
local single_quote = patterns.literal("'")
local double_quote = patterns.literal('"')


local _addition_operator = patterns.capture(patterns.literal("+"))
local _string_concat_operator = patterns.capture(patterns.literal('..'))

local identifier = patterns.one_or_more(patterns.branch(
  letter,
  underscore
))

local grammar = token.define(function(_ENV)
  START "vim9script"

  SUPPRESS(
    "ArithmeticTokens"
    , "ArithmeticExpression"
    , "StringExpression"
    , "ValidLine"

    , "_VarName"
    , "_PrimitiveExpression"
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
          , patterns.concat(V("FuncCall"), V("CapturedEOL"))
          , patterns.concat(patterns.any_amount(whitespace), V("CapturedEOL"))
          , V("Command")
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

  ArithmeticTokens = patterns.optional_surrounding_parenths(V("_PrimitiveExpression"))

  ArithmeticExpression = patterns.concat(
    V("ArithmeticTokens"),
    patterns.one_or_more(
      patterns.concat(
        V("AdditionOperator"),
        V("ArithmeticTokens")
      )
    )
  )

  StringOperator = patterns.capture(patterns.concat(
    any_whitespace,
    _string_concat_operator,
    any_whitespace
  ))

  StringExpression = patterns.concat(
    V("_PrimitiveExpression"),
    patterns.one_or_more(patterns.concat(
      V("StringOperator"),
      V("_PrimitiveExpression")
    ))
  )

  -- TODO: Probably need to handle escaping here...
  -- TODO: Lots of other things are strings.
  _InnerString = patterns.any_amount(
    patterns.branch(
      letter
      , whitespace
      , patterns.literal(":")
    )
  )

  String = patterns.capture(
    patterns.branch(
      patterns.concat(single_quote, V("_InnerString"), single_quote),
      patterns.concat(double_quote, V("_InnerString"), double_quote)
    )
  )

  -- StringConcat patterns.concat(

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
    any_whitespace,
    patterns.one_or_no(patterns.concat(
      patterns.literal("call"),
      some_whitespace
    )),
    V("FuncName"),
    left_paren,
    any_whitespace,
    V("FuncCallArgList"),
    any_whitespace,
    right_paren
  ))

  Boolean = patterns.capture(patterns.branch(
    patterns.literal("true"),
    patterns.literal("v:true"),
    patterns.literal("false"),
    patterns.literal("v:false")
  ))

  _PrimitiveExpression = patterns.branch(
    V("Number")
    , V("Boolean")
    , V("String")
    , V("FuncCall")
    , V("_VarName")
  )

  ConditionalExpression = patterns.concat(
    V("_PrimitiveExpression"),
    any_whitespace,
    patterns.literal("?"),
    any_whitespace,
    V("_PrimitiveExpression"),
    any_whitespace,
    patterns.literal(":"),
    any_whitespace,
    V("_PrimitiveExpression")
  )

  Expression = patterns.branch(
    V("ConditionalExpression")
    , V("ArithmeticExpression")
    , V("StringExpression")
    , V("_PrimitiveExpression")
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
    any_whitespace
    , patterns.literal("set")
    , some_whitespace
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
    , V("FuncCall")
    , V("Command")

    -- TODO: Not exactly true...
    , V("CapturedEOL")
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

  CommandName = patterns.capture(patterns.concat(
    -- TODO: Should make all the special words not allowed here.
    patterns.neg_look_ahead(patterns.branch(
      patterns.literal("let")
      , patterns.literal("end")
      , patterns.literal("call")
    )),
    V("_VarName")
  ))
  CommandArguments = patterns.capture(patterns.one_or_more(V("Expression")))
  CommandBang = patterns.capture(patterns.literal("!"))

  Command = patterns.capture(patterns.concat(
    any_whitespace,
    V("CommandName"),
    patterns.one_or_no(V("CommandBang")),
    patterns.one_or_no(patterns.concat(
      some_whitespace,
      V("CommandArguments")
    )),
    EOL_or_EOF
  ))


  UnparsedError = (1 - EOL) ^ 1
  UnparsedCapturedError = patterns.capture(V("UnparsedError"))
end)

return {
  grammar = grammar
}
