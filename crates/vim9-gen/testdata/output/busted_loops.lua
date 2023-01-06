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

  it('Test_can_do_for_loop', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    for _, y in vim9.iter({ 1, 2, 3 }) do
      x = vim9.ops.Plus(x, y)
    end

    vim9.fn.assert_equal(6, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_can_do_for_loop_with_break', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    for _, y in vim9.iter({ 1, 2, 3 }) do
      x = vim9.ops.Plus(x, y)

      if y == 2 then
        break
      end
    end

    vim9.fn.assert_equal(3, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_can_do_for_loop_with_break_with_continue', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    local body = function(_, y)
      x = vim9.ops.Plus(x, y)

      if y == 2 then
        return vim9.ITER_CONTINUE
      end

      if y == 3 then
        return vim9.ITER_BREAK
      end

      return vim9.ITER_DEFAULT
    end

    for _, y in vim9.iter({ 1, 2, 3, 4, 5 }) do
      local nvim9_status, nvim9_ret = body(_, y)
      if nvim9_status == vim9.ITER_BREAK then
        break
      elseif nvim9_status == vim9.ITER_RETURN then
        return nvim9_ret
      end
    end

    vim9.fn.assert_equal(6, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_can_do_for_loop_with_continue', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    local body = function(_, y)
      if y == 2 then
        return vim9.ITER_CONTINUE
      end

      x = vim9.ops.Plus(x, y)

      return vim9.ITER_DEFAULT
    end

    for _, y in vim9.iter({ 1, 2, 3 }) do
      local nvim9_status, nvim9_ret = body(_, y)
      if nvim9_status == vim9.ITER_BREAK then
        break
      elseif nvim9_status == vim9.ITER_RETURN then
        return nvim9_ret
      end
    end

    vim9.fn.assert_equal(4, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it('Test_can_do_for_loop_with_return', function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test

    local Something = function()
      local x = 0

      local body = function(_, y)
        if y == 2 then
          return vim9.ITER_RETURN, 5
        end

        if y == 10 then
          return vim9.ITER_CONTINUE
        end

        x = vim9.ops.Plus(x, y)

        return vim9.ITER_DEFAULT
      end

      for _, y in vim9.iter({ 1, 2, 3 }) do
        local nvim9_status, nvim9_ret = body(_, y)
        if nvim9_status == vim9.ITER_BREAK then
          break
        elseif nvim9_status == vim9.ITER_RETURN then
          return nvim9_ret
        end
      end

      return 0
    end

    vim9.fn.assert_equal(5, Something())

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return M
