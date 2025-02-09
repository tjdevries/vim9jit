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

  it('Test_same_line', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = vim9.convert.decl_bool(true and false)

    vim9.fn.assert_equal(x, false)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_next_line', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local y = vim9.convert.decl_bool(true and false)

    vim9.fn.assert_equal(y, false)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_missing_lines', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local z = true or false

    vim9.fn.assert_equal(z, true)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
