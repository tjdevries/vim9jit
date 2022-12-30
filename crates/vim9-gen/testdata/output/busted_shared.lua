local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_syn = function()
    pcall(vim.cmd, [[ syn keyword Test testkeyword contained ]])
    NVIM9.fn["assert_equal"](2, NVIM9.fn["len"](NVIM9.fn["split"](NVIM9.fn["execute"]("syntax list Test"), "\n")))
  end
end)
