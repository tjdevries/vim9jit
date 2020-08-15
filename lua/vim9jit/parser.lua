-- local l = require('lpeg')

package.loaded['vim9jit.p'] = nil
package.loaded['vim9jit.token'] = nil

local p = require('vim9jit.patterns')
local token = require('vim9jit.token')


-- capture
-- concat
-- any_amount
-- one_or_more
-- set

local Capture = p.capture
local Concat = p.concat

local CaptureSeq = function(...)
  return Capture(Concat(...))
end



local V = p.V

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

local EOL = p.end_of_line
local EOL_or_EOF = p.branch(EOL, p.end_of_file)

local digit = p.range('0', '9')
local letter = p.branch(
  p.range('a', 'z'),
  p.range('A', 'Z')
)

local comma = p.literal(",")
local underscore = p.literal("_")
local left_paren = p.literal("(")
local right_paren = p.literal(")")
local single_quote = p.literal("'")
local double_quote = p.literal('"')
local left_bracket = p.literal("[")
local right_bracket = p.literal("]")


local _addition_operator = Capture(p.literal("+"))
local _string_concat_operator = Capture(p.literal('..'))

local identifier = Concat(
  p.branch(
    letter,
    underscore
  ),
  p.any_amount(p.branch(
    letter,
    underscore,
    digit
  ))
)

local grammar = token.define(function(_ENV)
  START "vim9script"

  SUPPRESS(
    "ArithmeticTokens"
    , "ArithmeticExpression"
    , "StringExpression"
    , "ValidLine"

    , "_VarName"
    , "_PrimitiveExpression"
    , "_SimpleExpression"
  )

  vim9script = CaptureSeq(
    p.literal("vim9script"), EOL_or_EOF,
    p.any_amount(
      Concat(
        p.branch(
          V("FuncDef")
          , V("IfStatement")
          , V("For")
          , V("Let")
          , V("Assign")
          , Concat(V("FuncCall"), V("CapturedEOL"))
          , Concat(p.any_amount(whitespace), V("CapturedEOL"))
          , V("Command")
          , V("Comment")
        )
      )
    ),
    p.branch(
      p.one_or_no(p.end_of_file)
      -- , V("UnparsedCapturedError")
    )
  )

  CapturedEOL = CaptureSeq(any_whitespace, EOL)

  Number = Capture(p.branch(
    p.concat(p.one_or_more(digit), p.literal('.'), p.one_or_more(digit)),
    p.one_or_more(digit)
  ))

  VariableIdentifier = Capture(identifier)
  GlobalVariableIdentifier = CaptureSeq(
    p.literal("g:"),
    V("VariableIdentifier")
  )
  VimVariableIdentifier = CaptureSeq(
    p.literal("v:"),
    V("VariableIdentifier")
  )

  PrimitivesDictIdentifier = Capture(p.branch(
    p.literal("b"),
    p.literal("t"),
    p.literal("w")
  ))
  PrimitivesVariableIdentifier = CaptureSeq(
    V("PrimitivesDictIdentifier"),
    p.literal(":"),
    V("VariableIdentifier")
  )

  AdditionOperator = CaptureSeq(
    any_whitespace,
    _addition_operator,
    any_whitespace
  )

  ArithmeticTokens = p.optional_surrounding_parenths(V("_PrimitiveExpression"))

  ArithmeticExpression = Concat(
    V("ArithmeticTokens"),
    p.one_or_more(
      Concat(
        V("AdditionOperator"),
        V("ArithmeticTokens")
      )
    )
  )

  StringOperator = CaptureSeq(
    any_whitespace,
    _string_concat_operator,
    any_whitespace
  )

  StringExpression = Concat(
    V("_PrimitiveExpression"),
    p.one_or_more(Concat(
      V("StringOperator"),
      V("_PrimitiveExpression")
    ))
  )

  -- TODO: Probably need to handle escaping here...
  -- TODO: Lots of other things are strings.
  _InnerString = p.any_amount(
    p.branch(
      letter
      , whitespace
      , p.literal(":")
    )
  )

  String = Capture(
    p.branch(
      Concat(single_quote, V("_InnerString"), single_quote),
      Concat(double_quote, V("_InnerString"), double_quote)
    )
  )

  -- StringConcat Concat(

  FuncCallArg = Capture(
    p.branch(
      V("Expression")
      , V("VariableIdentifier")
    )
  )
  FuncCallArgList = Capture(
    p.one_or_no(Concat(
      V("FuncCallArg"),
      p.any_amount(Concat(
        any_whitespace, comma, any_whitespace
        , V("FuncCallArg")
      ))
    ))
  )

  FuncCall = CaptureSeq(
    any_whitespace,
    p.one_or_no(Concat(
      p.literal("call"),
      some_whitespace
    )),
    V("FuncName"),
    left_paren,
    any_whitespace,
    V("FuncCallArgList"),
    any_whitespace,
    right_paren
  )

  Boolean = Capture(p.branch(
    p.literal("true"),
    p.literal("v:true"),
    p.literal("false"),
    p.literal("v:false")
  ))

  _PrimitiveExpression = p.branch(
    V("Number")
    , V("Boolean")
    , V("String")
    , V("FuncCall")
    , V("_VarName")
  )

  _SimpleExpression = p.branch(
    V("ArithmeticExpression")
    , V("StringExpression")
    , V("ListExpression")
    , V("_PrimitiveExpression")
  )

  ConditionalExpression = CaptureSeq(
    V("_SimpleExpression"),
    any_whitespace_or_eol,
    p.literal("?"),
    any_whitespace_or_eol,
    V("_SimpleExpression"),
    any_whitespace_or_eol,
    p.literal(":"),
    any_whitespace_or_eol,
    V("_SimpleExpression")
  )

  ListExpression = CaptureSeq(
    left_bracket,
    any_whitespace,
    p.one_or_no(Concat(
      V("Expression"),
      p.any_amount(Concat(
        any_whitespace,
        comma,
        any_whitespace,
        V("Expression")
      )
    ),
    p.one_or_no(comma)
    )),
    any_whitespace,
    right_bracket
  )

  Expression = p.branch(
    V("ConditionalExpression")
    , V("_SimpleExpression")
  )

  TypeDefinition = Concat(
    p.literal(":"),
    p.any_amount(whitespace),
    Capture(p.any_amount(letter))
  )

  _VarName = p.branch(
    V("GlobalVariableIdentifier"),
    V("VimVariableIdentifier"),
    V("PrimitivesVariableIdentifier"),
    V("VariableIdentifier")
  )

  CommentChar = Capture(p.literal("#"))

  Comment = CaptureSeq(
    any_whitespace,
    V("CommentChar"),
    p.branch(
      p.up_to(EOL_or_EOF),
      EOL_or_EOF
    )
  )

  Let = CaptureSeq(
    p.any_amount(whitespace),
    p.literal("let"),
    p.one_or_more(whitespace),
    V("_VarName"),
    p.one_or_no(V("TypeDefinition")),
    p.one_or_no(Concat(
      p.one_or_more(whitespace),
      p.literal("="),
      p.any_amount(whitespace),
      V("Expression")
    )),
    EOL_or_EOF
  )

  Assign = CaptureSeq(
    -- TODO: Maybe could put this whitespace into the resulting lua so it isn't so ugly...
    p.any_amount(whitespace),
    V("_VarName"),
    p.one_or_more(whitespace),
    p.literal("="),
    p.any_amount(whitespace),
    V("Expression"),
    EOL_or_EOF
  )

  Set = CaptureSeq(
    any_whitespace
    , p.literal("set")
    , some_whitespace
  )

  -- TODO: This needs more options
  ForVar = Capture(
    V("_VarName")
  )

  ForObj = Capture(p.branch(
    V("FuncCall")
    , V("_VarName")
  ))

  ForBody = Capture(p.any_amount(V("ValidLine")))

  For = CaptureSeq(
    p.any_amount(whitespace)
    , p.literal("for")
        , some_whitespace
        , V("ForVar")
        , some_whitespace
        , p.literal("in")
        , some_whitespace
        , V("ForObj"), any_whitespace, EOL
      , V("ForBody")
    , any_whitespace, p.literal("endfor")
  )

  ReturnValue = Capture(V("Expression"))
  Return = CaptureSeq(
    any_whitespace,
    p.literal("return"),
    any_whitespace,
    V("ReturnValue")
  )

  -- {{{ If
  IfStatement = CaptureSeq(
    any_whitespace,
    "if", some_whitespace, V("Expression"), any_whitespace, EOL,
      V("IfBody"),
    any_whitespace_or_eol, "endif", any_whitespace_or_eol
  )

  IfBody = Capture(p.any_amount(V("ValidLine")))
  -- }}}

  ValidLine = p.branch(
    V("FuncDef")
    , V("IfStatement")
    -- NOTE: You probably shouldn't be able to return unless you're in a function...
    , V("Return")
    , V("For")
    , V("Let")
    , V("Assign")
    , V("Set")
    , V("FuncCall")
    , V("Command")
    , V("Comment")

    -- TODO: Not exactly true...
    , V("CapturedEOL")
  )

  FuncArg = Capture(
    V("_VarName")
  )

  FuncArgList = CaptureSeq(
    V("FuncArg"),
    p.any_amount(Concat(
      p.any_amount(whitespace),
      comma,
      p.any_amount(whitespace),
      V("FuncArg")
    ))
  )

  FuncBody = Capture(p.branch(
    p.one_or_more(V("ValidLine"))
    -- , p.any_amount(V("UnparsedCapturedError"))
  ))

  FuncName = Capture(V("_VarName"))

  FuncDef = CaptureSeq(
    p.any_amount(whitespace),
    p.literal("def"),
    p.one_or_more(whitespace),
    V("FuncName"),
    left_paren,
    p.one_or_no(V("FuncArgList")),
    right_paren,
    p.one_or_no(V("TypeDefinition")),
    any_whitespace, EOL,
    p.one_or_no(V("FuncBody")),
    any_whitespace,
    p.literal("enddef"), EOL_or_EOF
  )

  CommandName = CaptureSeq(
    -- TODO: Should make all the special words not allowed here.
    p.neg_look_ahead(p.branch(
      p.literal("let")
      , p.literal("end")
      , p.literal("call")
      , p.literal("def")
      , p.literal("if")
    )),
    V("_VarName")
  )
  CommandArguments = Capture(p.one_or_more(V("Expression")))
  CommandBang = Capture(p.literal("!"))

  Command = CaptureSeq(
    any_whitespace,
    V("CommandName"),
    p.one_or_no(V("CommandBang")),
    p.one_or_no(Concat(
      some_whitespace,
      V("CommandArguments")
    )),
    EOL_or_EOF
  )


  UnparsedError = (1 - EOL) ^ 1
  UnparsedCapturedError = Capture(V("UnparsedError"))
end)

return {
  grammar = grammar
}
