function! HelloWorld()
  return "Hello World"
endfunction

lua << EOF
require('vim9jit')

_G[ [=[vim.fn.expand('<SID>') .. "__" .. "FinalFunc"]=] ] = function()
  return vim.fn.HelloWorld()
end
EOF

function! FinalFunc()
  return luaeval("_G[ [=[" . expand('<SID>') . "__FinalFunc]=] ]()")
endfunction

let g:MY_VAR = FinalFunc()
echo g:MY_VAR
