local helpers = require('test.helpers')
local eq = assert.are.same
local get_item = helpers.get_item

local grammar = require('vim9jit.parser_2').make_grammar()

local parse = function(s)
  return require('vim9jit.grammar').match(
    grammar, 'vim9script\n' .. helpers.dedent(s)
  )
end

local match = function(t)
  local parsed = parse(t[2])

  if t.attr then
    eq(t[1], parsed[t.attr])
  else
    eq(t[1], parsed)
  end
end

local pit = function(name, str, checks)
  it(name, function()
    local parsed = parse(str)
    checks(parsed, function(id)
      return get_item(parsed, 'id', id)
    end)
  end)
end


describe('parser', function()
  it('should load something', function()
    match {
      1, '', attr = 'pos'
    }
  end)

  pit('shoudl load another something', 'var = var', function(parsed, get)
    eq('var', get('Expression').value)
  end)
end)
