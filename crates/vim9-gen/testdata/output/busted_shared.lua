----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

-- Ignore "value assigned to a local variable is unused" because
--  we can't guarantee that local variables will be used by plugins
-- luacheck: ignore 311
--- @diagnostic disable

local vim9 = require('_vim9script')
local M = {}
describe('filename', function()
  -- vim9script

  it('Test_syn', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    pcall(vim.cmd, [[ syn keyword Test testkeyword contained ]])
    vim9.fn.assert_equal(2, vim9.fn.len(vim9.fn.split(vim9.fn.execute('syntax list Test'), '\n')))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
