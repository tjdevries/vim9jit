local __VIM9_MODULE = {}
describe("filename", function()
  -- vim9script

  it("Test_operations", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool5 = require("vim9script").convert.decl_bool(require("vim9script").ops["And"](1, true))
    require("vim9script").fn["assert_equal"](true, bool5)
    local bool6 = require("vim9script").convert.decl_bool(require("vim9script").ops["And"](0, 1))
    require("vim9script").fn["assert_equal"](false, bool6)
    local bool7 = require("vim9script").convert.decl_bool(
      require("vim9script").ops["And"](require("vim9script").ops["Or"](0, 1), true)
    )
    require("vim9script").fn["assert_equal"](true, bool7)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
