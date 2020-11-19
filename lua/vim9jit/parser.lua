-- local l = require('lpeg')

package.loaded['vim9jit.patterns'] = nil
package.loaded['vim9jit.token'] = nil

local p = require('vim9jit.patterns')
local token = require('vim9jit.token')


-- capture
-- concat
-- any_amount
-- one_or_more
-- set

p.capture_seq = function(...)
  return p.capture(p.concat(...))
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
local left_brace = p.literal("{")
local right_brace = p.literal("}")
local single_quote = p.literal("'")
local double_quote = p.literal('"')
local left_bracket = p.literal("[")
local right_bracket = p.literal("]")


local _addition_operator = p.capture(p.literal("+"))
local _string_concat_operator = p.capture(p.literal('..'))

local identifier = p.concat(
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

local list_comma = p.concat(
  any_whitespace,
  comma,
  any_whitespace
)

local colon = p.concat(
  any_whitespace,
  p.literal(":"),
  any_whitespace
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

  vim9script = p.capture_seq(
    p.literal("vim9script"), EOL_or_EOF,
    p.any_amount(
      p.branch(
        V("FuncDef")
        , V("IfStatement")
        , V("For")
        , V("Var")
        , V("Assign")
        , p.concat(V("FuncCall"), V("CapturedEOL"))
        , p.concat(p.any_amount(whitespace), V("CapturedEOL"))
        , V("Command")
        , V("Comment")
      )
    ),
    p.branch(
      p.one_or_no(p.end_of_file)
      -- , V("UnparsedCapturedError")
    )
  )

  CapturedEOL = p.capture_seq(any_whitespace, EOL)

  Number = p.capture(p.branch(
    p.concat(
      p.branch(p.literal("0x"), p.literal("0X")),
      p.one_or_more(p.branch(
        digit, p.range('a', 'f'), p.range('A', 'F')
      ))
    ),
    p.concat(p.one_or_more(digit), p.literal('.'), p.one_or_more(digit)),
    p.one_or_more(digit)
  ))

  VariableIdentifier = p.capture(identifier)
  GlobalVariableIdentifier = p.capture_seq(
    p.literal("g:"),
    V("VariableIdentifier")
  )
  VimVariableIdentifier = p.capture_seq(
    p.literal("v:"),
    V("VariableIdentifier")
  )

  PrimitivesDictIdentifier = p.capture(p.branch(
    p.literal("b"),
    p.literal("t"),
    p.literal("w")
  ))
  PrimitivesVariableIdentifier = p.capture_seq(
    V("PrimitivesDictIdentifier"),
    p.literal(":"),
    V("VariableIdentifier")
  )

  AdditionOperator = p.capture_seq(
    any_whitespace,
    _addition_operator,
    any_whitespace
  )

  ArithmeticTokens = p.optional_surrounding_parenths(V("_PrimitiveExpression"))

  ArithmeticExpression = p.concat(
    V("ArithmeticTokens"),
    p.one_or_more(
      p.concat(
        V("AdditionOperator"),
        V("ArithmeticTokens")
      )
    )
  )

  StringOperator = p.capture_seq(
    any_whitespace,
    _string_concat_operator,
    any_whitespace
  )

  StringExpression = p.concat(
    V("_PrimitiveExpression"),
    p.one_or_more(p.concat(
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

  String = p.capture(
    p.branch(
      p.concat(single_quote, V("_InnerString"), single_quote),
      p.concat(double_quote, V("_InnerString"), double_quote)
    )
  )

  -- Stringp.concat Concat(

  FuncCallArg = p.capture(
    p.branch(
      V("Expression")
      , V("VariableIdentifier")
    )
  )
  FuncCallArgList = p.capture(
    p.one_or_no(p.concat(
      V("FuncCallArg"),
      p.any_amount(p.concat(
        any_whitespace, comma, any_whitespace
        , V("FuncCallArg")
      ))
    ))
  )

  FuncCall = p.capture_seq(
    any_whitespace,
    p.one_or_no(p.concat(
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

  --[[
    TODO:
      - Need to add arrow methods for objects in vimL.
        Difficulty is that these can be different parameters, different semantics,
        different EVERYTHING for them... Getting this to compile would be VERY difficult.

        myList->add(3)
  --]]
  MethodCall = p.capture_seq(
    V("_VarName"),
    "->",
    V("FuncName"),
    left_paren,
    V("FuncCallArgList"),
    right_paren
  )

  Boolean = p.capture(p.branch(
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
    , V("MethodCall")
    , V("_VarName")
  )

  _SimpleExpression = p.branch(
    V("ArithmeticExpression")
    , V("StringExpression")
    , V("ListExpression")
    , V("DictExpression")
    , V("_PrimitiveExpression")
  )

  ConditionalExpression = p.capture_seq(
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

  ListExpression = p.capture_seq(
    left_bracket,
    any_whitespace,
    p.one_or_no(p.concat(
      V("Expression"),
      p.any_amount(p.concat(
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

  DictKey = p.capture_seq(
    V("String")
  )

  DictValue = p.capture_seq(
    V("Expression")
  )

  DictExpression = p.capture_seq(
    left_brace,
    any_whitespace,
    p.any_amount(p.concat(
      V("DictKey"),
      colon,
      V("DictValue"),
      p.one_or_no(list_comma)
    )),
    any_whitespace,
    right_brace
  )

  Expression = p.branch(
    V("ConditionalExpression")
    , V("_SimpleExpression")
  )


  --[[
  The following builtin types are supported:
      bool
      number
      float
      string
      blob
      list<{type}>
      dict<{type}>
      job
      channel
      func
      func: {type}
      func({type}, ...)
      func({type}, ...): {type}
  --]]
  _Type_IdentifierDefinition = p.one_or_more(letter)

  _Type_GenericDefintion = p.concat(
    p.branch(
      -- TODO: We could switch this at some point to be just letters.
      --        But I don't know if that's even valid vim9.
      p.literal("list")
      , p.literal("dict")
    )
    , p.literal('<')
    , V("_TypeSyntax")
    , p.literal('>')
  )

  _Type_FunctionDefintion = p.concat(
    p.literal("func"),
    p.branch(
      p.concat(
        p.literal(":")
      ),
      p.concat(
        left_paren
        , any_whitespace
        , p.branch(
          p.concat(
            p.list_of(V("_TypeSyntax"), list_comma),
            p.one_or_no(p.concat(list_comma, p.literal('...')))
          ),
          p.literal('...')
        )
        , any_whitespace
        , p.literal('):')
      )
    )
    , any_whitespace
    , V("_TypeSyntax")
  )

  _Type_SingleDefinition = p.branch(
    V("_Type_GenericDefintion")
    , V("_Type_FunctionDefintion")
    , V("_Type_IdentifierDefinition")
  )

  _TypeSyntax = p.list_of(V("_Type_SingleDefinition"), p.literal("|"))

  TypeDefinition = p.concat(
    colon,
    p.capture(V("_TypeSyntax"))
  )

  _VarName = p.branch(
    V("GlobalVariableIdentifier"),
    V("VimVariableIdentifier"),
    V("PrimitivesVariableIdentifier"),
    V("VariableIdentifier")
  )

  CommentChar = p.capture(p.literal("#"))

  Comment = p.capture_seq(
    any_whitespace,
    V("CommentChar"),
    p.neg_look_ahead(left_brace),
    p.branch(
      p.up_to(EOL_or_EOF),
      EOL_or_EOF
    )
  )

  --[[
  TODO: initialize to correct value... this might be harder
    Declaring a variable with a type but without an initializer will initialize to
    zero, false or empty.
  --]]
  Var = p.capture_seq(
    p.any_amount(whitespace),
    p.branch(
      p.literal("var"),
      p.literal("const"),
      p.literal("final")
    ),
    p.one_or_more(whitespace),
    V("_VarName"),
    p.one_or_no(V("TypeDefinition")),
    p.one_or_no(p.concat(
      p.one_or_more(whitespace),
      p.literal("="),
      p.any_amount(whitespace),
      V("Expression")
    )),
    EOL_or_EOF
  )

  --[[
    TODO:
      - Since `&opt = value` is now assigning a value to option "opt", ":&" cannot be
        used to repeat a `:substitute` command.

        Need to handle transforming `&opt = value` -> either vim.api.nvim_set_<optionalname>_option or
        just execute as command and hope we can transform the value correctly...
        Probably would work with `vim.cmd(string.format("let &opt = %s", <whatever value is here>))`
        or similar.
  --]]
  Assign = p.capture_seq(
    -- TODO: Maybe could put this whitespace into the resulting lua so it isn't so ugly...
    p.any_amount(whitespace),
    V("_VarName"),
    p.one_or_more(whitespace),
    p.literal("="),
    p.any_amount(whitespace),
    V("Expression"),
    EOL_or_EOF
  )

  Set = p.capture_seq(
    any_whitespace
    , p.literal("set")
    , some_whitespace
  )

  -- TODO: This needs more options
  ForVar = p.capture(
    V("_VarName")
  )

  ForObj = p.capture(p.branch(
    V("FuncCall")
    , V("_VarName")
  ))

  ForBody = p.capture(p.any_amount(V("ValidLine")))

  For = p.capture_seq(
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

  ReturnValue = p.capture(V("Expression"))
  Return = p.capture_seq(
    any_whitespace,
    p.literal("return"),
    any_whitespace,
    V("ReturnValue")
  )

  -- {{{ If
  IfStatement = p.capture_seq(
    any_whitespace,
    "if", some_whitespace, V("Expression"), any_whitespace, EOL,
      V("IfBody"),
    p.one_or_no(V("ElseStatement")),
    any_whitespace_or_eol,
    "endif", any_whitespace_or_eol
  )

  ElseStatement = p.capture_seq(
    any_whitespace, "else", any_whitespace, EOL, V("IfBody")
  )

  IfBody = p.capture(p.any_amount(V("ValidLine")))
  -- }}}

  ValidLine = p.branch(
    V("FuncDef")
    , V("IfStatement")
    -- NOTE: You probably shouldn't be able to return unless you're in a function...
    , V("Return")
    , V("For")
    , V("Var")
    , V("Assign")
    , V("Set")
    , V("FuncCall")
    , V("Command")
    , V("Comment")

    -- TODO: Not exactly true...
    , V("CapturedEOL")
  )

  FuncArg = p.capture_seq(
    V("_VarName"),
    p.one_or_no(
      V("TypeDefinition"),
      any_whitespace
    )
  )

  FuncArgList = p.capture_seq(
    V("FuncArg"),
    p.any_amount(p.concat(
      p.any_amount(whitespace),
      comma,
      p.any_amount(whitespace),
      V("FuncArg")
    ))
  )

  FuncBody = p.capture(p.branch(
    p.one_or_more(V("ValidLine"))
    -- , p.any_amount(V("UnparsedCapturedError"))
  ))

  FuncName = p.capture(V("_VarName"))

  FuncDef = p.capture_seq(
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

  CommandName = p.capture_seq(
    -- TODO: Should make all the special words not allowed here.
    p.neg_look_ahead(p.branch(
      p.literal("var")
      , p.literal("final")
      , p.literal("const")
      , p.literal("end")
      , p.literal("call")
      , p.literal("def")
      , p.literal("if")
      , p.literal("else")
      , p.literal("endif")
    )),
    V("_VarName")
  )
  CommandArguments = p.capture(p.one_or_more(V("Expression")))
  CommandBang = p.capture(p.literal("!"))

  Command = p.capture_seq(
    any_whitespace,
    V("CommandName"),
    p.one_or_no(V("CommandBang")),
    p.one_or_no(p.concat(
      some_whitespace,
      V("CommandArguments")
    )),
    EOL_or_EOF
  )


  UnparsedError = (1 - EOL) ^ 1
  UnparsedCapturedError = p.capture(V("UnparsedError"))
end)

return {
  grammar = grammar
}
