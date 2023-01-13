----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

-- Ignore "value assigned to a local variable is unused" because
--  we can't guarantee that local variables will be used by plugins
-- luacheck: ignore 311

local vim9 = require('_vim9script')
local M = {}
describe('filename', function()
  -- vim9script

  it('Test_naked_g', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim.g['test_var'] = 5
    vim9.fn.assert_equal(5, vim9.fn.get(vim.g, 'test_var'))
    vim9.fn.assert_equal(6, vim9.fn.get(vim.g, 'fake_var', 6))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
