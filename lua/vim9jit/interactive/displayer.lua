RELOAD('vim9jit')

local make_grammar = require('vim9jit.parser_2').make_grammar

local bufnr = 234

local input = "5+ 5*2"
local root = "Expression"

local grammar = make_grammar(root)
local parsed = grammar:match(input)

vim.api.nvim_buf_set_lines(bufnr, 0, -1, false,
  vim.split(vim.inspect(parsed), "\n")
)

