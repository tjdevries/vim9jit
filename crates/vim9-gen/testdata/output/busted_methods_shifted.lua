----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

local NVIM9 = require('_vim9script')
describe('filename', function()
  it('Test_settabvar_method', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn['settabvar'](1, 'testing', 0)
    NVIM9.fn['assert_equal'](0, vim.t['testing'])

    NVIM9.fn['settabvar'](1, 'testing', 25)
    NVIM9.fn['assert_equal'](25, vim.t['testing'])

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
