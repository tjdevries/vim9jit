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

  it('Test_assignment_one', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool1 = vim9.convert.decl_bool(true)
    vim9.fn.assert_equal(vim.v['true'], bool1)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
