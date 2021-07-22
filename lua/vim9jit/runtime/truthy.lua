Vim9__Truthy = function(val)
  local t_val = type(val)
  if t_val == "boolean" then
    return val
  elseif t_val == "number" then
    return val ~= 0
  end
end

return Vim9__Truthy
