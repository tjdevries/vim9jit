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
  it('Test_settabvar_method', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    vim9.fn.settabvar(1, 'testing', 0)
    vim9.fn.assert_equal(0, vim.t['testing'])

    vim9.fn.settabvar(1, 'testing', 25)
    vim9.fn.assert_equal(25, vim.t['testing'])

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
