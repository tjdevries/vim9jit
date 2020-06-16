
vim9jit = {}

vim9jit.DefaultForType = function(type_str)
  if type_str == 'number' then
    return 0
  else
    error(string.format('Unknown type_str: %s', type_str))
  end
end

return vim9jit
