----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

-- Ignore "value assigned to a local variable is unused" because
--  we can't guarantee that local variables will be used by plugins
-- luacheck: ignore 311

local vim9 = require('_vim9script')
local M = {}
local l = nil
describe('filename', function()
  -- vim9script

  l = { 1, 2, 3 }

  it('Test_can_index', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal(1, vim9.index(l, 0))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_minus', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local index = 1
    vim9.fn.assert_equal(1, vim9.index(l, vim9.ops.Minus(index, 1)))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_both', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal({ 1, 2 }, vim9.slice(l, 0, 1))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_left', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal({ 2, 3 }, vim9.slice(l, 1, nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_right', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal({ 1, 2 }, vim9.slice(l, nil, 1))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_index_with_prefix_spaced', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal({ 3 }, vim9.slice(l, -1, nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_index_with_prefix', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.assert_equal({ 3 }, vim9.slice(l, -1, nil))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_string', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = 'abcd'
    vim9.fn.assert_equal(vim9.slice(foo, nil, -2), 'abc')

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
