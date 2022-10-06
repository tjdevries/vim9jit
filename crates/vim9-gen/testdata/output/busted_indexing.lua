local __VIM9_MODULE = {}
describe("filename", function()
  -- vim9script
  local l = { 1, 2, 3 }

  -- Token(EndOfLine, "\n", (3,0)->(3,0))

  it("Test_can_index", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    require("vim9script").fn["assert_equal"](1, require("vim9script").index(l, 0))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (7,0)->(7,0))

  it("Test_both", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    require("vim9script").fn["assert_equal"]({ 1, 2 }, require("vim9script").slice(l, 0, 1))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (11,0)->(11,0))

  it("Test_left", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    require("vim9script").fn["assert_equal"]({ 2, 3 }, require("vim9script").slice(l, 1, nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (15,0)->(15,0))

  it("Test_right", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    require("vim9script").fn["assert_equal"]({ 1, 2 }, require("vim9script").slice(l, nil, 1))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
