local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_can_index = nil
  local Test_minus = nil
  local Test_both = nil
  local Test_left = nil
  local Test_right = nil
  local Test_index_with_prefix_spaced = nil
  local Test_index_with_prefix = nil
  local Test_string = nil
  -- vim9script
  local l = { 1, 2, 3 }

  it("Test_can_index", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"](1, NVIM9.index(l, 0))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_minus", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local index = 1
    NVIM9.fn["assert_equal"](1, NVIM9.index(l, NVIM9.ops["Minus"](index, 1)))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_both", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"]({ 1, 2 }, NVIM9.slice(l, 0, 1))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_left", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"]({ 2, 3 }, NVIM9.slice(l, 1, nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_right", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"]({ 1, 2 }, NVIM9.slice(l, nil, 1))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_index_with_prefix_spaced", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"]({ 3 }, NVIM9.slice(l, NVIM9.prefix["Minus"](1), nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_index_with_prefix", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"]({ 3 }, NVIM9.slice(l, NVIM9.prefix["Minus"](1), nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_string", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = "abcd"
    NVIM9.fn["assert_equal"](NVIM9.slice(foo, nil, NVIM9.prefix["Minus"](2)), "abc")

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
