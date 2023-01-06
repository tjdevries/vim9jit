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

  it('Test_simple_heredoc', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { [==[hello]==], [==[world]==] }

    vim9.fn.assert_equal({ 'hello', 'world' }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_simple_heredoc_with_whitespace', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { [==[    hello]==], [==[  world]==] }

    vim9.fn.assert_equal({ '    hello', '  world' }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_simple_heredoc_with_no_whitespace_trim', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = vim9.heredoc.trim({ [==[    hello]==], [==[world]==] })

    vim9.fn.assert_equal({ '    hello', 'world' }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_simple_heredoc_with_whitespace_trim', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = vim9.heredoc.trim({ [==[        hello]==], [==[          world]==] })

    vim9.fn.assert_equal({ 'hello', '  world' }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- # def Test_simple_heredoc_with_quotes()
  -- #   var x =<< END
  -- #     "hello"
  -- #   world
  -- #   END
  -- #
  -- #   assert_equal(["    "hello"", "  world"], x)
  -- # enddef
end)
return M
