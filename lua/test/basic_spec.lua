local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local helpers = require('test.helpers')
local eq, neq, get_item = helpers.eq, helpers.neq, helpers.get_item


local make_vim9script = function(text)
  return 'vim9script\n' .. text
end

describe('parser', function()
  it('should load vim9script file', function()
    local parsed = token.parsestring(grammar, "vim9script")

    eq(parsed.id, 'vim9script')
  end)

  it('should parse simple let statements', function()
    local parsed = token.parsestring(grammar, make_vim9script("let x = 1"))
    neq(nil, parsed)

    local let = get_item(parsed, 'id', 'Let')

    eq(let.id, 'Let')
  end)
end)
