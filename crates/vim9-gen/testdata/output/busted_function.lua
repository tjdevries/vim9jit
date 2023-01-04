----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

-- Ignore "value assigned to a local variable is unused" because
--  we can't guarantee that local variables will be used by plugins
-- luacheck: ignore 311

local vim9 = require('_vim9script')
describe('filename', function()
  -- vim9script

  it('Test_default_args', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test

    local MyCoolFunc = function(x)
      x = vim.F.if_nil(x, 5, x)
      return x
    end

    vim9.fn.assert_equal(MyCoolFunc(), 5)
    vim9.fn.assert_equal(MyCoolFunc(10), 10)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_inplace', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local explicit = { 3, 2, 1 }
    explicit = vim9.fn_mut('sort', { explicit }, { replace = 0 })
    vim9.fn.assert_equal({ 1, 2, 3 }, explicit)

    local inplace = { 3, 2, 1 }
    vim9.fn_mut('sort', { inplace }, { replace = 0 })
    vim9.fn.assert_equal({ 1, 2, 3 }, inplace)

    local expr_sort = vim9.fn.sort({ 3, 2, 1 })
    vim9.fn.sort({ 3, 2, 1 })
    vim9.fn.assert_equal({ 1, 2, 3 }, expr_sort)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_insert_inplace', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 2, 3 }
    local bar = vim9.fn.insert(foo, 4, vim9.fn.len(foo))
    vim9.fn.assert_equal(foo, bar)

    vim9.fn.insert(bar, 5, vim9.fn.len(bar))
    vim9.fn.assert_equal({ 1, 2, 3, 4, 5 }, bar)
    vim9.fn.assert_equal(foo, bar)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_insert_inplace', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 3, 2 }
    vim9.fn_mut('reverse', { vim9.fn_mut('sort', { foo }, { replace = 0 }) }, { replace = 0 })
    vim9.fn.assert_equal({ 3, 2, 1 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
