local NVIM9 = require("_vim9script")
describe("filename", function()
  Test_settabvar_method = function()
    NVIM9.fn["settabvar"](1, "testing", 0)
    NVIM9.fn["assert_equal"](0, vim.t["testing"])

    NVIM9.fn["settabvar"](1, "testing", 25)
    NVIM9.fn["assert_equal"](25, vim.t["testing"])
  end
end)
