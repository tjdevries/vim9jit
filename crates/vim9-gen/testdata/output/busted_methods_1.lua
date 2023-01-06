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
  it('Test_method_newline', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 2, 3 }
    vim9.fn_mut('filter', {
      x,
      function(_, y)
        return y == 2
      end,
    }, { replace = 0 })

    vim9.fn.assert_equal({ 2 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
