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

  it('Test_assignment_bool_1', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool1 = vim9.convert.decl_bool(true)
    vim9.fn.assert_equal(vim.v['true'], bool1)
    local bool2 = vim9.convert.decl_bool(false)
    vim9.fn.assert_equal(vim.v['false'], bool2)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_assignment_bool_2', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool3 = vim9.convert.decl_bool(0)
    vim9.fn.assert_equal(false, bool3)
    local bool4 = vim9.convert.decl_bool(1)
    vim9.fn.assert_equal(true, bool4)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_assignment_bool_3', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool5 = vim9.convert.decl_bool(vim9.ops.And(1, true))
    vim9.fn.assert_equal(true, bool5)
    local bool6 = vim9.convert.decl_bool(vim9.ops.And(0, 1))
    vim9.fn.assert_equal(false, bool6)
    local bool7 = vim9.convert.decl_bool(vim9.ops.Or(0, vim9.ops.And(1, true)))
    vim9.fn.assert_equal(true, bool7)

    -- # var lines =<< trim END
    -- #   vim9script
    -- #   def GetFlag(): bool
    -- #     var flag: bool = 1
    -- #     return flag
    -- #   enddef
    -- #   var flag: bool = GetFlag()
    -- #   assert_equal(true, flag)
    -- #   flag = 0
    -- #   assert_equal(false, flag)
    -- #   flag = 1
    -- #   assert_equal(true, flag)
    -- #   flag = 1 || true
    -- #   assert_equal(true, flag)
    -- #   flag = 1 && false
    -- #   assert_equal(false, flag)

    -- #   var cp: bool = &cp
    -- #   var fen: bool = &l:fen
    -- # END
    -- # v9.CheckScriptSuccess(lines)
    -- # v9.CheckDefAndScriptFailure(['var x: bool = 2'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = -1'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = [1]'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = {}'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = "x"'], 'E1012:')

    -- # v9.CheckDefAndScriptFailure(['var x: bool = "x"', '', 'eval 0'], 'E1012:', 1)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_unpacked_identifiers', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x, y = unpack({ 1, 2 })
    vim9.fn.assert_equal(1, x)
    vim9.fn.assert_equal(2, y)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_modifier_prefixes', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = 10
    foo = foo - 1
    vim9.fn.assert_equal(9, foo)

    foo = foo + 1
    foo = foo + 1
    vim9.fn.assert_equal(11, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_using_reserved_words', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local __end__ = true
    vim9.fn.assert_equal(__end__, true)

    local __M__ = true
    vim9.fn.assert_equal(__M__, true)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
