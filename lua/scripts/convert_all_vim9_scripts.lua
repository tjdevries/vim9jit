require('plenary.reload').reload_module('vim9jit')

local generate = require('vim9jit.generator').generate

local convert_file = function(filename)
  if string.find(filename, "/_", 1, true) then
    print("Skipping", filename)
    return
  end

  print("Parsing and generating for:", filename)
  local file_io = io.open(filename, "r")
  local lines = file_io:read("a")
  file_io:close()

  local new_lua = generate(lines)
  print(new_lua)
  print("=============")

  local new_file = vim.fn.fnamemodify(filename, ":r") .. '.lua'

  local out_io = io.open(new_file, "w")
  out_io:write("--[=[\n")
  out_io:write("Original vimscript\n")
  out_io:write(lines)
  out_io:write("--]=]")
  out_io:write("\n\n")
  out_io:write(new_lua)
  out_io:close()
end

for _, filename in ipairs(vim.fn.glob('./vim9_scripts/**.vim', nil, true)) do
  convert_file(filename)
end

for _, filename in ipairs(vim.fn.glob('./vim9_scripts/testdir/*.vim', nil, true)) do
  convert_file(filename)
end
