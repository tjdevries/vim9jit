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

convert.decl_dict = function(val)
  if type(val) == "nil" then
    return vim.empty_dict()
  elseif type(val) == "table" then
    if vim.tbl_isempty(val) then
      return vim.empty_dict()
    elseif vim.tbl_islist(val) then
      error(string.format("Cannot pass list to dictionary? %s", vim.inspect(val)))
    else
      return val
    end
  end

  error(string.format("invalid dict declaration: %s", vim.inspect(val)))
end

convert.to_vim_bool = function(val)
  if type(val) == "boolean" then
    return val
  elseif type(val) == "number" then
    return val ~= 0
  elseif type(val) == "string" then
    return string.len(val) ~= 0
  elseif type(val) == "table" then
    return not vim.tbl_isempty(val)
  elseif val == nil then
    return false
  end

  error("unhandled type: " .. vim.inspect(val))
end

return convert
