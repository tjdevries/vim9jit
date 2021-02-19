RELOAD('vim9jit')

local transpiler = require('vim9jit.transpiler')

local output = transpiler.transpile [[
vim9script

def MyFunc(): bool
  var x: bool = 1

  return x
enddef

echo MyFunc()
]]

local partial_output = transpiler.transpile [[
echo "Old Vimscript Is Still A Thing"

def MyFunc(): bool
  var x: bool = 1

  return x
enddef

echo MyFunc()
]]

--[[
echo "Old Vimscript Is Still A Thing"

lua << EOF

function script__MyFunc()
  local x = Vim9__Truthy(1)
  return x
end

EOF

function g:MyFunc()
  return v:lua.SCRIPT_MyFunc()
endfunction

echo MyFunc()
--]]

-- print(output)

local result = vim.api.nvim_exec(output, true)
print("Result: ", result)
