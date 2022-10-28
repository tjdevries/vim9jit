local NVIM9 = require("_vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_same_line = nil
  local Test_next_line = nil
  local Test_missing_lines = nil
  -- vim9script

  it("Test_same_line", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = NVIM9.convert.decl_bool(true and false)

    NVIM9.fn["assert_equal"](x, false)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_next_line", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local y = NVIM9.convert.decl_bool(true and false)

    NVIM9.fn["assert_equal"](y, false)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_missing_lines", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local z = true or false

    NVIM9.fn["assert_equal"](z, true)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
