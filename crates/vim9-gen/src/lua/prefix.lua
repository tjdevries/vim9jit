local lib = require('_vim9script')

local M = {}

M['Minus'] = function(right)
  return -right
end

M['Bang'] = function(right)
  return not lib.bool(right)
end

return M
