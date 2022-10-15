vim9script

# No arguments
var no_args = bufnr()

# One argument
var some_args = len("hello world")

# More arguments
var matched = match(["matched", "some_args"], "a", 1)

var zed = symbolTable[mid].name->len()
prop_add(symbolTable[mid].outlineLine, col,
          {bufnr: bnr, type: 'LspOutlineHighlight',
          length: symbolTable[mid].name->len()})
