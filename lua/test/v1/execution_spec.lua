local helpers = require('test.helpers')
local eq = helpers.eq
local assert_execute = helpers.assert_execute

VimFnCount = 0
vim.fn = setmetatable({}, {
  __index = function(t, key)
    local function _fn(...)
      VimFnCount = VimFnCount + 1
      return vim.call(key, ...)
    end
    t[key] = _fn
    return _fn
  end
})


describe('execute generated vimscript', function()
  before_each(function()
    VimFnCount = 0
  end)

  describe('assignment', function()
    it('should allow simple assignment', function()
      assert_execute(5, "var RESULT = 5")
      assert.are.same(VimFnCount, 0)
    end)

    it('should allow simple addition', function()
      assert_execute(10, "var RESULT = 5 + 5")
      assert.are.same(VimFnCount, 0)
    end)

    it('should work with global variables', function()
      assert_execute(function() return vim.api.nvim_get_var("glob_var") end, [[
        var RESULT = true
        var g:glob_var = RESULT
      ]])
    end)

    describe('defaults', function()
      it('number', function()
        assert_execute(0, 'var RESULT: number')
      end)
    end)
  end)

  it('should return strings', function()
    assert_execute("Hello, world!", [[var RESULT = "Hello, world!"]])
  end)

  it('should return lists', function()
    assert_execute({1, 2, 3}, "var RESULT = [1, 2, 3]")
  end)

  describe('Expression', function()
    describe('ComparisonExpression', function()
      it('should work for numbers', function()
        assert_execute(true, "var RESULT = 1 == 1")
        assert_execute(false, "var RESULT = 2 == 1")
      end)

      it('should work for strings', function()
        assert_execute(true, [[var RESULT = 'hello' == "hello"]])
        assert_execute(false, [[var RESULT = 'hello' == 'world']])
      end)

      it('should work for weird vim string comparison operators', function()
        assert_execute(false, [[var RESULT = 'hello' ==  'hElLo']])
        assert_execute(false, [[var RESULT = 'hello' ==# 'hElLo']])
        assert_execute(true,  [[var RESULT = 'hello' ==? 'hElLo']])
      end)
    end)

    describe('BinaryExpression', function()
      describe('&&', function()
        it('bools and bools', function()
          assert_execute(true, [[var RESULT = true && true]])
          assert_execute(false, [[var RESULT = false && true]])
          assert_execute(false, [[var RESULT = false && false]])
        end)

        it('numbers and bools', function()
          assert_execute(true, [[var RESULT = 1 && true]])
        end)

        it('can have definitions as well', function()
          assert_execute(true, [[var RESULT: bool = 1 && true]])
        end)
      end)
    end)
  end)

  describe('Math', function()
    -- We haven't gotten this to work yet.
    pending('works with precedence', function()
      assert_execute(12, [[var RESULT = 2 + 2 * 5]])
      assert_execute(12, [[var RESULT = 2 * 5 + 2]])
    end)
  end)

  describe('IfStatement', function()
    it('work with else and forward decl', function()
      assert_execute({1, 2, 3}, [[
        var RESULT
        if true
          RESULT = [1, 2, 3]
        else
          RESULT = "NOT VALID"
        endif
      ]])
    end)
  end)

  describe('Function', function()
    it('should generate a local function', function()
      assert_execute(2, [[
        var RESULT = 0

        def VimNew()
          RESULT = RESULT + 1
        enddef

        VimNew()
        VimNew()
      ]])

      eq(0, VimFnCount)
    end)

    it('should call vim builtin functions', function()
      assert_execute({1, 2, 3, 4}, [[
        var RESULT = range(1, 4)
      ]])

      eq(1, VimFnCount)
    end)
  end)

  describe('MethodCall', function()
    it('works with myList->add', function()
      assert_execute({1, 2, 3, 'new'}, [[
        var RESULT = [1, 2, 3]
        RESULT = RESULT->add('new')
      ]])

      eq(0, VimFnCount)
    end)

    it('works with myList->add, declared inline', function()
      assert_execute({1, 2, 3, 'new'}, [[
        var RESULT = [1, 2, 3]->add('new')
      ]])
    end)

    it('works with filter, string', function()
      assert_execute({2}, [[var RESULT = [1, 2, 3]->filter('v:val % 2 == 0')]])

      eq(1, VimFnCount)
    end)

    it('works with filter, string', function()
      assert_execute({2}, [[
        var RESULT = [1, 2, 3]->filter({v -> v == 2})
      ]])

      eq(0, VimFnCount)
    end)

    it('works with filter, string', function()
      assert_execute({3}, [[
        var RESULT = [1, 2, 3]->filter({ -> v:val == 3})
      ]])

      eq(1, VimFnCount)
    end)

    it('works with multipe method calls', function()
      --[[
Actually generates this basically:

local RESULT = (function()
local __TSMethodCall1 = { 1, 2, 3 }
local __TSMethodCall2 = (table.insert(__TSMethodCall1, 4) or __TSMethodCall1)
local __TSMethodCall3 = vim.fn['filter'](__TSMethodCall2, 'v:val % 2 == 0')
return __TSMethodCall3
end)()
      --]]
      assert_execute({2, 4}, [[var RESULT = [1, 2, 3]->add(4)->filter('v:val % 2 == 0')]])
      eq(1, VimFnCount)
    end)

    pending('calls vim.fn.filter less often when possible', function()
      assert_execute({2, 4}, [[var RESULT = [1, 2, 3]->add(4)->filter({idx, val -> fmod(val, 2)})]])
      eq(0, VimFnCount)
    end)
  end)

  describe('For loops', function()
    pending('should be able to use pairs wrapper', function()
      assert_execute(10, [[
        var RESULT = 0
        var adder = 1

        def MyFunc(a, b)
          return range(1, 10)
        enddef

        for i in MyFunc(1, 10)
          RESULT = RESULT + adder
        endfor
      ]])
    end)

    it('should not call vimscript for numeric ranges', function()
      assert_execute(10, [[
        var RESULT = 0
        for i in range(1, 10)
          RESULT = RESULT + 1
        endfor
      ]])

      eq(0, VimFnCount)
    end)
  end)
end)
