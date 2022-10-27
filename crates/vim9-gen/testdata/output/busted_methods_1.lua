local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_method_newline = nil

  it("Test_method_newline", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 2, 3 }
    NVIM9.fn_mut("filter", {
      x,
      function(_, y)
        return NVIM9.ops["EqualTo"](y, 2)
      end,
    }, { replace = 0 })

    NVIM9.fn["assert_equal"]({ 2 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
