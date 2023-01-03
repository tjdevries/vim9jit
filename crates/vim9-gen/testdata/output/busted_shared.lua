----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

local NVIM9 = require('_vim9script')
describe('filename', function()
  -- vim9script

  it('Test_syn', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    pcall(vim.cmd, [[ syn keyword Test testkeyword contained ]])
    NVIM9.fn['assert_equal'](
      2,
      NVIM9.fn['len'](NVIM9.fn['split'](NVIM9.fn['execute']('syntax list Test'), '\n'))
    )

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
