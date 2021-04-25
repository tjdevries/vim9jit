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
      eq({1, "hello"}, eval('[1 ,"hello"  ]', true))
    end)

    it('gets the right math results', function()
      eq(10, eval('5 + 5'))
      eq(47, eval('5 + 6*7'))
      eq(47, eval('5 + 6   *7   '))
    end)
  end)
end)
