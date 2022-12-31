local NVIM9 = require("_vim9script")
describe("filename", function()
  it("Test_method_comments", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 2, 3 }
    NVIM9.fn_mut("filter", { x, function(_, y)
      return y == 2
    end }, { replace = 0 })

    NVIM9.fn["assert_equal"](x, { 2 })

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
