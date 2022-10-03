describe("filename", function()
  -- vim9script

  it("Test_assignment_one", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool1 = require("vim9script").convert.decl_bool(true)
    vim.fn["assert_equal"](vim.v["true"], bool1)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
