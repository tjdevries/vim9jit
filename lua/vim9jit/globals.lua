vim9jit = {}

vim9jit.DefaultForType = function(type_str)
  error("Unimplemented: DefaultForType")
end

vim9jit.VimPairs = function(tbl)
  if vim.tbl_islist(tbl) then
    return ipairs(tbl)
  else
    return pairs(tbl)
  end
end

vim9jit.conditional = function(cond, if_true, if_false)
  if cond then
    return if_true
  else
    return if_false
  end
end

vim9jit.vim_function = function(name)
  return function(...)
    vim.api.nvim_call_function(name, {...})
  end
end

return vim9jit
