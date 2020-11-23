require('plenary.test_harness'):setup_busted()

local expression_grammar = require('vim9jit.parser').expression_grammar
local token = require('vim9jit.token')

local generator = require('vim9jit.generator')

local helpers = require('test.helpers')
local eq, neq, get_item = helpers.eq, helpers.neq, helpers.get_item

local get_parsed = function(s)
  return token.parsestring(expression_grammar, s)
end

local eval = function(s)
  if not vim then
    return
  end

  return loadstring('return ' .. generator.generate(s))()
end


generator._utils.set_grammar(expression_grammar)

describe("expressions", function()
  it('parses a single number', function()
    neq(nil, get_parsed('1'))
  end)

  it('parses a single number from an emacs programmer', function()
    neq(nil, get_parsed('(1)'))
  end)

  describe("evaluate", function()
    it('returns a single number', function()
      eq(1, eval('1'))
    end)

    it('returns a single var', function()
      eq(true, eval('true'))
      eq(true, eval('v:true'))
      eq(false, eval('false'))
      eq(false, eval('v:false'))
    end)

    it('returns a list', function()
      eq({1, 2}, eval('[1 ,2]'))
    end)
  end)
end)
