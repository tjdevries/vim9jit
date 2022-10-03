" This will break vim9 if used in this way
"   We can't hold the references for these
"   (there is no way to know)
function! Hello(mylist)
  return a:mylist
endfunction

let y = [1, 2, 3]
let x = Hello(y)

let foo = [1, 2, 3]
let bar = insert(foo, 4, len(foo))
echo foo
echo bar
