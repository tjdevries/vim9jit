vim.fn.assert_equal = assert.are.equal

describe("simple_assign.vim", function()
  it("Test_assignment_one", function()
    -- generated code here
    local bool1 = true
    vim.fn.assert_equal(true, bool1)
  end)
end)
