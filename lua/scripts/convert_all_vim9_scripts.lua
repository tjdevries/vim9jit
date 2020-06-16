
package.loaded['vim9jit'] = nil
package.loaded['vim9jit.parser'] = nil
package.loaded['vim9jit.generator'] = nil

local generate = require('vim9jit.generator').generate

for _, filename in ipairs(vim.fn.glob('./vim9_scripts/*.vim', nil, true)) do
  print("Parsing and generating for:", filename)
  local file_io = io.open(filename, "r")
  local lines = file_io:read("a")
  ile_io:close()

  local new_lua = generate(lines)

  local new_file = vim.fn.fnamemodify(filename, ":r") .. '.lua'

  local out_io = io.open(new_file, "w")
  out_io:write(new_lua)
  out_io:close()
end
