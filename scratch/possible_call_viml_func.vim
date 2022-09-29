function! VIM9__CallUserVimlFunc(name, args) abort
  let ret = function(a:name, a:args)()
  return [ret, a:args]
endfunction
