local convert = {}

convert.decl_bool = function(val)
  if type(val) == "boolean" then
    return val
  elseif type(val) == "number" then
    if val == 0 then
      return false
    elseif val == 1 then
      return true
    else
      error(string.format("bad number passed to bool declaration: %s", val))
    end
  end

  error("invalid bool declaration: %s", vim.inspect(val))
end

return convert
