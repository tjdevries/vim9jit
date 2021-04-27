local generator = require('vim9jit.generator')

local helpers = require('test.helpers')
local eq = helpers.eq

local eval = function(s, force)
  local generated, parsed = generator.generate(s, 'Expression')
  local inner = vim.split(generated, "\n")
  inner[#inner] = "return " .. inner[#inner]

  local to_load = string.format([[
    return (function()
      %s
    end)()
  ]], table.concat(inner, "\n"))

  local to_call = loadstring(to_load)
  if not to_call or force then
    error(string.format("Failed: %s\n\n%s\n", to_load, vim.inspect(parsed)))
    return
  end

  return to_call()
end

describe("expressions", function()
  describe("evaluate", function()
    it('returns a single number', function()
      eq(1, eval('1'))
      eq(1, eval('(1)'))
      eq(1, eval('(((((1)))))'))
    end)

    it('returns global variables', function()
      vim.api.nvim_set_var('example', true)
      eq(true, eval('g:example'))
      vim.api.nvim_set_var('example', false)
      eq(false, eval('g:example'))
    end)

    it('returns a string', function()
      eq("hello", eval('"hello"'))
    end)

    it('returns a single var', function()
      eq(true, eval('true'))
      eq(true, eval('v:true'))
      eq(false, eval('false'))
      eq(false, eval('v:false'))
    end)

    it('returns a list', function()
      eq({1, 2}, eval('[1 ,2]'))
      eq({1, "hello"}, eval('[1 ,"hello"  ]'))

      vim.api.nvim_set_var('example', true)
      eq({true}, eval('[g:example]'))
    end)

    it('gets the right math results', function()
      eq(10, eval('5 + 5'))
      eq(47, eval('5 + 6*7'))
      eq(47, eval('5 + 6   *7   '))
      eq(11, eval('(5 + 6)'))
    end)

    it('can handle parenth exprssion', function()
      eq(77, eval('(5 + 6) *7'))
    end)

    describe('can call builtin functions', function()
      it('can return lists', function()
        eq({1, 2, 3, 4}, eval('range( (1), 4)'))
      end)

      it('can return dicts', function()
        eq({A = true}, eval('{A: true}'))
      end)

      it('can return strings', function()
        eq("hello", eval('trim("   hello  ")'))
      end)

      it('can interact with lists', function()
        eq(1, eval('get([1, 2, 3, 4], 0)'))
      end)

      it('can do addition with functions', function()
        eq(2, eval('1 + get([1, 2, 3, 4], 0)'))
        eq(3, eval('get([1, 2, 3, 4], 1) + 1'))
      end)
    end)

    describe('list access', function()
      it('can grab items from a list literal (must plus 1)', function()
        eq('b', eval('["a", "b", "c"][1]'))
      end)

      it('can grab items from a dictionary literal', function()
        eq(true, eval('{A: true}["A"]'))
      end)

      it('can grab items from a dictionary with complicated expr', function()
        eq(11, eval('{A: 5 + 6}["A"]'))
      end)
    end)
  end)
end)
