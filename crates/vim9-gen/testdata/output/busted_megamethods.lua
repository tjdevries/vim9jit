----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

-- Ignore "value assigned to a local variable is unused" because
--  we can't guarantee that local variables will be used by plugins
-- luacheck: ignore
--- @diagnostic disable

local vim9 = require('_vim9script')
local M = {}
describe('filename', function()
  -- vim9script

  it('Test_inplace_discarded', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal(
      { 2, 4 },
      vim9.fn.filter(vim9.fn.sort({ 1, 4, 2, 5 }), function(_, y)
        return vim9.ops.Modulo(y, 2) == 0
      end)
    )

    local foo = vim9.fn.filter(vim9.fn.sort({ 1, 4, 2, 5 }), function(_, y)
      return vim9.ops.Modulo(y, 2) == 0
    end)
    vim9.fn.assert_equal({ 2, 4 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_returned_foo', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    vim9.fn.assert_equal(
      { 5, 4, 2, 1 },
      vim9.fn_mut('reverse', { vim9.fn_mut('sort', { foo }, { replace = 0 }) }, { replace = 0 })
    )

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_returned_foo', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    local bar = foo
    vim9.fn_mut('reverse', { vim9.fn_mut('sort', { foo }, { replace = 0 }) }, { replace = 0 })
    vim9.fn.assert_equal({ 5, 4, 2, 1 }, foo)
    vim9.fn.assert_equal({ 5, 4, 2, 1 }, bar)
    vim9.fn.assert_equal(foo, bar)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
