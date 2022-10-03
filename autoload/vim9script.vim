function! vim9script#fn(name, args) abort
  let ret = function(a:name, a:args)()
  return [ret, a:args]
endfunction
