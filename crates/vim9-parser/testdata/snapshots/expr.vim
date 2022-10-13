vim9script

# TODO:
# echo x y z

var x = 10
var y = x
var z = +1
var foo = true
var sum = x + y
var parened = (((x + y)))
var if = true
var var = if
echo var

var x = v:false
echo x

command! -nargs=0 -bar LspGotoDefinition lsp.GotoDefinition(v:false)
# command! -nargs=0 -bar LspGotoDefinition lsp.GotoDefinition(v:false)