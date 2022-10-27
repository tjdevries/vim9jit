local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_settabvar_method = nil

  it("Test_settabvar_method", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["settabvar"](1, "testing", 0)
    NVIM9.fn["assert_equal"](0, vim.t["testing"])

    NVIM9.fn["settabvar"](1, "testing", 25)
    NVIM9.fn["assert_equal"](25, vim.t["testing"])

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
