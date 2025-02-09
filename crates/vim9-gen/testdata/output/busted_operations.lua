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

  it('Test_operations', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool5 = vim9.convert.decl_bool(vim9.ops.And(1, true))
    vim9.fn.assert_equal(true, bool5)
    local bool6 = vim9.convert.decl_bool(vim9.ops.And(0, 1))
    vim9.fn.assert_equal(false, bool6)
    local bool7 = vim9.convert.decl_bool(vim9.ops.Or(0, vim9.ops.And(1, true)))
    vim9.fn.assert_equal(true, bool7)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_lsp_from_yega', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = true
    if vim9.bool(vim9.ops.Or(vim9.prefix['Bang'](vim9.fn.has('vim9script')), 900 < 900)) then
      x = false
    end

    vim9.fn.assert_equal(true, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
