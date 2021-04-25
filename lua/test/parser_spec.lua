local helpers = require('test.helpers')
local eq = assert.are.same
local get_item = helpers.get_item

local grammar = require('vim9jit.parser').make_grammar()

local parse = function(s, g)
  local used_grammar = grammar
  if g then
    used_grammar = require('vim9jit.parser').make_grammar(g)
  end

  local prefix = ''
  if not g then
    prefix = 'vim9script\n' 
  end

  return require('vim9jit.grammar').match(
    used_grammar, prefix .. helpers.dedent(s)
  )
end

local match = function(t)
  local parsed = parse(t[2], t.g)

  if t.attr then
    eq(t[1], parsed[t.attr])
  else
    eq(t[1], parsed)
  end
end

local pit = function(name, str, checks, g)
  it(name, function()
    local parsed = parse(str, g)
    checks(parsed, function(id)
      return get_item(parsed, 'id', id)
    end)
  end)
end


describe('parser', function()
  it('should load something', function()
    match {
      'vim9script\n', '', attr = 'value'
    }
  end)

  pit('should load another something', 'var = var', function(parsed, get)
    eq('var', get('Expression').value)
  end)

  describe('numbers', function()
    local test_number_value = function(name, input, output)
      pit(name, input, function(parsed, get)
        if output ~= get('Number').value then
          P(parsed)
        end

        eq(output, get('Number').value)
      end, 'Number')
    end

    local test_failed_number_value = function(name, input)
      pit(name, input, function(parsed, _)
        eq(nil, parsed)
      end, 'Number')
    end

    test_number_value('should parse a decimal number', '5', '5')

    test_number_value('should parse a hex number: x', '0x10', '0x10')
    test_number_value('should parse a hex number: X', '0X10', '0X10')
    test_number_value('should parse a hex number: x', '0X1F', '0X1F')

    test_number_value('should parse float', '10.32', '10.32')
    test_number_value('should parse float', '0.5', '0.5')

    test_number_value('should parse negative number', '-1', '-1')
    test_number_value('should parse plus number', '+1', '+1')

    test_failed_number_value('should not parse double unary', '++5')
    test_failed_number_value('should not parse double unary', '+-5')
  end)

  describe('expressions', function()
    describe('arithmetic', function()
      -- '5 + 7 * 12'
      -- { '+', 5, { '*', 7, 12 } }

      -- vim9script
      -- [1, 2, 3] + [4, 5, 6] -> [1, 2, 3, 4, 5, 6]

      local test_expression = function(name, input, output)
        pit(name, input, function(parsed, get)
          if output ~= get('Expression').value then
            P(parsed)
          end

          eq(output, get('Expression').value)
        end, 'Expression')
      end

      test_expression("addition is fine", "5+5", "5+5")
      test_expression("addition with whitespace is fine", "5 +\t 5", "5 +\t 5")
    end)
  end)
end)
