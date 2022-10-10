local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_operations = nil
  -- vim9script

  it("Test_operations", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool5 = NVIM9.convert.decl_bool(NVIM9.ops["And"](1, true))
    NVIM9.fn["assert_equal"](true, bool5)
    local bool6 = NVIM9.convert.decl_bool(NVIM9.ops["And"](0, 1))
    NVIM9.fn["assert_equal"](false, bool6)
    local bool7 = NVIM9.convert.decl_bool(NVIM9.ops["And"](NVIM9.ops["Or"](0, 1), true))
    NVIM9.fn["assert_equal"](true, bool7)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
