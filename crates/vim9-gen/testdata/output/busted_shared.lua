describe("filename", function()
  -- vim9script

  it("Test_syn", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim.cmd([===[  syn keyword Test testkeyword contained]===])
    vim.fn["assert_equal"](2, vim.fn["len"](vim.fn["split"](vim.fn["execute"]("syntax list Test"), "\n")))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
