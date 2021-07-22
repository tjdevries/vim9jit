local file_mode = require "vim9jit.generator.modes.file"

local def_mode = setmetatable({}, {
  __index = file_mode,
})

def_mode.FuncStatement = function(mode, match)
end

return def_mode
