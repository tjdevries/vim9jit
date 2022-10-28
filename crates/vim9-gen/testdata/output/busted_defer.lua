local NVIM9 = require("_vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local MyDefer = nil
  local RangeDefer = nil
  local Test_defer = nil

  -- # def AddDefer(arg1)
  -- #   call extend(g:deferred, [arg1])
  -- #   # if a:0 == 1
  -- #   #   call extend(g:deferred, [1])
  -- #   # endif
  -- # enddef
  -- #
  -- # def WithDeferTwo()
  -- #   call extend(g:deferred, ['in Two'])
  -- #   for nr in range(3)
  -- #     defer AddDefer('Two' .. nr)
  -- #   endfor
  -- #   call extend(g:deferred, ['end Two'])
  -- # enddef
  -- #
  -- # def WithDeferOne()
  -- #   call extend(g:deferred, ['in One'])
  -- #   defer AddDefer('One')
  -- #   call WithDeferTwo()
  -- #   call extend(g:deferred, ['end One'])
  -- #
  -- #   # call writefile(['text'], 'Xfuncdefer')
  -- #   # defer delete('Xfuncdefer')
  -- # enddef
  -- #
  -- # def Test_defer()
  -- #   g:deferred = []
  -- #   call WithDeferOne()
  -- #
  -- #   call assert_equal(['in One', 'in Two', 'end Two', 'Two2', 'Two1', 'Two0', 'end One', 'One'], g:deferred)
  -- # enddef

  MyDefer = function()
    local nvim9_deferred = {}
    local _, ret = pcall(function()
      local x = {}
      table.insert(nvim9_deferred, 1, function()
        NVIM9.fn["add"](x, 1)
      end)

      table.insert(nvim9_deferred, 1, function()
        NVIM9.fn["add"](x, 2)
      end)

      table.insert(nvim9_deferred, 1, function()
        NVIM9.fn["add"](x, 3)
      end)

      return x
    end)

    for _, nvim9_defer in ipairs(nvim9_deferred) do
      pcall(nvim9_defer)
    end

    return ret
  end

  RangeDefer = function()
    local nvim9_deferred = {}
    local _, ret = pcall(function()
      local x = {}

      for _, i in NVIM9.iter(NVIM9.fn["range"](3)) do
        table.insert(nvim9_deferred, 1, function()
          NVIM9.fn["add"](x, i)
        end)
      end

      return x
    end)

    for _, nvim9_defer in ipairs(nvim9_deferred) do
      pcall(nvim9_defer)
    end

    return ret
  end

  it("Test_defer", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = MyDefer()
    NVIM9.fn["assert_equal"]({ 3, 2, 1 }, x)

    local y = RangeDefer()
    NVIM9.fn["assert_equal"]({ 3, 2, 1 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
