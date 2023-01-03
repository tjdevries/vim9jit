vim.cmd [[
function! _Vim9ScriptFn(name, args) abort
  try
    let ret = function(a:name, a:args)()
  catch
    echo "Failed..."
    echo a:name
    echo a:args

    throw v:errmsg
  endtry

  return [ret, a:args]
endfunction
]]
