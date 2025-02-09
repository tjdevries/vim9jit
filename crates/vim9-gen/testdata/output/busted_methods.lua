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

  it('Test_string_methods', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local s = 'hello'
    vim9.fn.assert_equal(vim9.fn.len(s), 5)

    vim9.fn.assert_equal(vim9.fn.len('hello'), 5)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_list_methods', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = vim9.fn.sort({ 5, 4, 2, 1, 7, 12, 8 })

    vim9.fn.assert_equal({ 1, 12, 2, 4, 5, 7, 8 }, x)

    local numeric = vim9.fn.sort({ 5, 4, 2, 1, 7, 12, 8 }, 'n')

    vim9.fn.assert_equal({ 1, 2, 4, 5, 7, 8, 12 }, numeric)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_inplace', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 4, 2, 5 }
    vim9.fn_mut('sort', { x }, { replace = 0 })

    vim9.fn.assert_equal({ 1, 2, 4, 5 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_inplace_inplace_multi', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 4, 2, 5 }
    vim9.fn_mut('sort', { x }, { replace = 0 })
    vim9.fn_mut('filter', {
      x,
      function(_, y)
        return vim9.ops.Modulo(y, 2) == 0
      end,
    }, { replace = 0 })
    vim9.fn.assert_equal({ 2, 4 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_inplace_inplace_single', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    vim9.fn_mut('filter', {
      vim9.fn_mut('sort', { foo }, { replace = 0 }),
      function(_, y)
        return vim9.ops.Modulo(y, 2) == 0
      end,
    }, { replace = 0 })

    vim9.fn.assert_equal({ 2, 4 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_method_precedence', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local server = { ['filetype'] = true }
    vim9.fn.assert_equal(false, vim9.prefix['Bang'](vim9.fn.has_key(server, 'filetype')))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
