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

  it('should parse simple let statements', function()
    local result = generate(make_vim9script("let x = 1 + 2"))

    eq('local x = 1 + 2', result)
  end)
end)
