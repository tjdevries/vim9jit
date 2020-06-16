local generator = require('vim9jit.generator')
local generate = generator.generate

local helpers = require('test.helpers')
local eq, neq, get_item = helpers.eq, helpers.neq, helpers.get_item

local make_vim9script = function(text)
  return 'vim9script\n' .. text
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
      let this_var: number = 1
      this_var = 3
    ]]))
    eq("local this_var = 1\nthis_var = 3", result)
  end)
end)
