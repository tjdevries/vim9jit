require('plenary.test_harness'):setup_busted()

local generator = require('vim9jit.generator')
local generate = generator.generate

local helpers = require('test.helpers')
local eq, neq, get_item = helpers.eq, helpers.neq, helpers.get_item

local make_vim9script = function(text)
  return 'vim9script\n' .. helpers.dedent(text)
end

describe('generator', function()
  it('should generate nothing for empty file', function()
    local result = generate("vim9script")

    eq('', result)
  end)

  it('should parse simple let statements', function()
    local result = generate(make_vim9script("let x = 1"))

    eq('local x = 1', result)
  end)

  it('should parse simple let addition statements', function()
    local result = generate(make_vim9script("let x = 1 + 2"))

    eq('local x = 1 + 2', result)
  end)

  it('should parse simple let addition statements', function()
    local result = generate(make_vim9script("let x: number"))

    eq('local x = vim9jit.DefaultForType("number")', result)
  end)

  it('should ignore type statements when strict mode is off', function()
    -- Our other option would be do something like `local x = assert_type(1 + 2, 'number')
    local result = generate(make_vim9script("let x: number = 1 + 2"))

    eq('local x = 1 + 2', result)
  end)

  it('should not ignore type statements when in strict mode', function()
    -- Our other option would be do something like `local x = assert_type(1 + 2, 'number')
    local result = generate(make_vim9script("let x: number = 1 + 2"), true)

    eq('local x = vim9jit.AssertType("number", 1 + 2)', result)
  end)

  it('should not use vim.g for declaring globals', function()
    local result = generate(make_vim9script("let g:glob_var = 1 + 2"))

    eq('vim.g["glob_var"] = 1 + 2', result)
  end)

  it('should not use local again for variables', function()
    local result = generate(make_vim9script([[
      let this_var = 1
      this_var = 3
    ]]))

    eq("local this_var = 1\nthis_var = 3", result)
  end)

  describe('calling functions', function()
    it('should redirect to vim functions', function()
      local result = generate(make_vim9script [[
        let range = range(1, 100)
      ]])

      eq("local range = vim.fn['range'](1, 100)", result)
    end)
  end)

  describe('loops', function()
    it('should use pairs wrapper', function()
      local result = generate(make_vim9script [[
        let sum = 0
        for i in my_func(1, 100)
          sum = sum + 1
        endfor
      ]])

      eq(vim.trim[[
local sum = 0
for _, i in vim9jit.VimPairs(vim.fn['my_func'](1, 100)) do
  sum = sum + 1
end
]], result)
    end)

    it('should use super cool range wrapper', function()
      local result = generate(make_vim9script [[
        let sum = 0
        for i in range(1, 100)
          sum = sum + 1
        endfor
      ]])

      eq(vim.trim[[
local sum = 0
for i = 1, 100, 1 do
  sum = sum + 1
end
]], result)
    end)
  end)
end)
