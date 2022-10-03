function! VIM9__CallUserVimlFunc(name, args) abort
  " TODO: We could return a list that checks whether the returned value is
  " any of the arguments passed in, to do this dynamically.

  let ret = function(a:name, a:args)()
  return [ret, a:args]
endfunction
