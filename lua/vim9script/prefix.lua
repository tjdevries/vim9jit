local to_vim_bool = require("vim9script.convert").to_vim_bool

local prefix = {}

prefix["Minus"] = function(right)
  return -right
end

prefix["Bang"] = function(right)
  return not to_vim_bool(right)
end

return prefix
