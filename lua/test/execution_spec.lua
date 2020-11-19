require('plenary.test_harness'):setup_busted()

local generator = require('vim9jit.generator')
local generate = generator.generate
local fmt = generator._utils.fmt

local helpers = require('test.helpers')
local eq = helpers.eq

local make_vim9script = function(text)
  return 'vim9script\n' .. fmt(text)
end

local execute = function(vim9_str)
  local compiled = generate(make_vim9script(vim9_str))
  local loaded = loadstring(compiled .. "\nreturn RESULT")

  local ok, result = pcall(loaded)
  if not ok then
    print("GENERATED:", compiled)
    print("   FROM  :", make_vim9script(vim9_str))
    print("LOADED FAILURE", loaded, loaded_failure)
  end

  return result, compiled
end

local assert_execute = function(expected, vim9_str)
  local result, compiled = execute(vim9_str)

  if not pcall(eq, expected, result) then
    print(string.rep("=", 80))
    print("COMPILED:", tostring(compiled))
    eq(expected, result)
  end
end

describe('execute generated vimscript', function()
  it('should return simple numbers', function()
    assert_execute(5, "var RESULT = 5")
  end)

  it('should return strings', function()
    assert_execute("Hello, world!", [[var RESULT = "Hello, world!"]])
  end)

  it('should return lists', function()
    assert_execute({1, 2, 3}, "var RESULT = [1, 2, 3]")
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

  describe('MethodCall', function()
    it('works with myList->add', function()
      assert_execute({1, 2, 3, 'new'}, [[
        var RESULT = [1, 2, 3]
        RESULT = RESULT->add('new')
      ]])
    end)

    it('works with myList->add, declared inline', function()
      assert_execute({1, 2, 3, 'new'}, [[
        var RESULT = [1, 2, 3]->add('new')
      ]])
    end)

    it('works with filter', function()
      assert_execute({2}, [[var RESULT = [1, 2, 3]->filter('v:val % 2 == 0')]])
    end)

    it('works with multipe method calls', function()
      --[[
Actually generates this basically:

local RESULT = (function()
local __TSMethodCall1 = { 1, 2, 3 }
local __TSMethodCall2 = vim.fn['add'](__TSMethodCall1, 4)
local __TSMethodCall3 = vim.fn['filter'](__TSMethodCall2, 'v:val % 2 == 0')
return __TSMethodCall3
end)()
      --]]
      assert_execute({1, 2, 4}, [[var RESULT = [1, 2, 3]->add(4)->filter('v:val % 2 == 0')]])
    end)
  end)
end)
