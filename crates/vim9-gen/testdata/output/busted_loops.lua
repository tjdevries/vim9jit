local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_can_do_for_loop = nil
  local Test_can_do_for_loop_with_break = nil
  local Test_can_do_for_loop_with_break_with_continue = nil
  local Test_can_do_for_loop_with_continue = nil
  local Test_can_do_for_loop_with_return = nil
  -- vim9script

  it("Test_can_do_for_loop", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    for _, y in NVIM9.iter({ 1, 2, 3 }) do
      x = NVIM9.ops["Plus"](x, y)
    end

    NVIM9.fn["assert_equal"](6, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_can_do_for_loop_with_break", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    for _, y in NVIM9.iter({ 1, 2, 3 }) do
      x = NVIM9.ops["Plus"](x, y)

      if NVIM9.bool(y == 2) then
        break
      end
    end

    NVIM9.fn["assert_equal"](3, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_can_do_for_loop_with_break_with_continue", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    local body = function(_, y)
      x = NVIM9.ops["Plus"](x, y)

      if NVIM9.bool(y == 2) then
        return NVIM9.ITER_CONTINUE
      end

      if NVIM9.bool(y == 3) then
        return NVIM9.ITER_BREAK
      end

      return NVIM9.ITER_DEFAULT
    end

    for _, y in NVIM9.iter({ 1, 2, 3, 4, 5 }) do
      local nvim9_status, nvim9_ret = body(_, y)
      if nvim9_status == NVIM9.ITER_BREAK then
        break
      elseif nvim9_status == NVIM9.ITER_RETURN then
        return nvim9_ret
      end
    end

    NVIM9.fn["assert_equal"](6, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_can_do_for_loop_with_continue", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = 0

    local body = function(_, y)
      if NVIM9.bool(y == 2) then
        return NVIM9.ITER_CONTINUE
      end

      x = NVIM9.ops["Plus"](x, y)

      return NVIM9.ITER_DEFAULT
    end

    for _, y in NVIM9.iter({ 1, 2, 3 }) do
      local nvim9_status, nvim9_ret = body(_, y)
      if nvim9_status == NVIM9.ITER_BREAK then
        break
      elseif nvim9_status == NVIM9.ITER_RETURN then
        return nvim9_ret
      end
    end

    NVIM9.fn["assert_equal"](4, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_can_do_for_loop_with_return", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test

    local Something = function()
      local x = 0

      local body = function(_, y)
        if NVIM9.bool(y == 2) then
          return NVIM9.ITER_RETURN, 5
        end

        if NVIM9.bool(y == 10) then
          return NVIM9.ITER_CONTINUE
        end

        x = NVIM9.ops["Plus"](x, y)

        return NVIM9.ITER_DEFAULT
      end

      for _, y in NVIM9.iter({ 1, 2, 3 }) do
        local nvim9_status, nvim9_ret = body(_, y)
        if nvim9_status == NVIM9.ITER_BREAK then
          break
        elseif nvim9_status == NVIM9.ITER_RETURN then
          return nvim9_ret
        end
      end

      return 0
    end

    NVIM9.fn["assert_equal"](5, Something())

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
