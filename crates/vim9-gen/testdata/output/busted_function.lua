local NVIM9 = require("_vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_default_args = nil
  local Test_inplace = nil
  local Test_insert_inplace = nil
  local Test_insert_inplace = nil
  -- vim9script

  it("Test_default_args", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test

    local MyCoolFunc = function(x)
      x = vim.F.if_nil(x, 5, x)
      return x
    end

    NVIM9.fn["assert_equal"](MyCoolFunc(), 5)
    NVIM9.fn["assert_equal"](MyCoolFunc(10), 10)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local explicit = { 3, 2, 1 }
    explicit = NVIM9.fn_mut("sort", { explicit }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 1, 2, 3 }, explicit)

    local inplace = { 3, 2, 1 }
    NVIM9.fn_mut("sort", { inplace }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 1, 2, 3 }, inplace)

    local expr_sort = NVIM9.fn["sort"]({ 3, 2, 1 })
    NVIM9.fn["sort"]({ 3, 2, 1 })
    NVIM9.fn["assert_equal"]({ 1, 2, 3 }, expr_sort)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_insert_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 2, 3 }
    local bar = NVIM9.fn["insert"](foo, 4, NVIM9.fn["len"](foo))
    NVIM9.fn["assert_equal"](foo, bar)

    NVIM9.fn["insert"](bar, 5, NVIM9.fn["len"](bar))
    NVIM9.fn["assert_equal"]({ 1, 2, 3, 4, 5 }, bar)
    NVIM9.fn["assert_equal"](foo, bar)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_insert_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 3, 2 }
    NVIM9.fn_mut("reverse", { NVIM9.fn_mut("sort", { foo }, { replace = 0 }) }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 3, 2, 1 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
