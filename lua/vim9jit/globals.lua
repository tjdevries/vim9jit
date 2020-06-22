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

return vim9jit
