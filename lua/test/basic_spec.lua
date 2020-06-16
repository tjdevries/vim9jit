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

  it('should parse simple addition statements', function()
    local parsed = token.parsestring(grammar, make_vim9script("let x = 1 + 2"))
    neq(nil, parsed)

    local let = get_item(parsed, 'id', 'Let')

    eq(let.id, 'Let')
    eq(let.value, 'let x = 1 + 2')
  end)

  it('should parse simple type definitions', function()
    local parsed = token.parsestring(grammar, make_vim9script("let x: number = 1 + 2"))
    neq(nil, parsed)

    local let = get_item(parsed, 'id', 'Let')

    eq(let.id, 'Let')
    eq(let.value, 'let x: number = 1 + 2')

    local type_definition = get_item(parsed, 'id', 'TypeDefinition')
    neq(nil, type_definition)
    eq(type_definition.value, 'number')
  end)

  it('should allow for type definition with no assign', function()
    local parsed = token.parsestring(grammar, make_vim9script("let x: number"))
    neq(nil, parsed)

    local let = get_item(parsed, 'id', 'Let')

    eq(let.id, 'Let')
    eq(let.value, 'let x: number')

    local type_definition = get_item(parsed, 'id', 'TypeDefinition')
    neq(nil, type_definition)
    eq(type_definition.value, 'number')
  end)

  it('should parse handle global variables', function()
    local parsed = token.parsestring(grammar, make_vim9script("let g:glob_var = 1"))
    neq(nil, parsed)

    local let = get_item(parsed, 'id', 'Let')
    eq(let.id, 'Let')

    local global_var = get_item(let, 'id', 'GlobalVariableIdentifier')
    neq(nil, global_var)
    eq(global_var.value, 'g:glob_var')

    local var = get_item(global_var, 'id', 'VariableIdentifier')
    eq(var.value, 'glob_var')
  end)

  it('should allow updating an existing variable', function()
    local parsed = token.parsestring(grammar, make_vim9script([[
      let this_var: number = 1
      this_var = 3
    ]]))
    neq(nil, parsed)

    local var = get_item(get_item(parsed, 'id', 'Let'), 'id', 'VariableIdentifier')
    eq(var.id, 'VariableIdentifier')
    eq(var.value, 'this_var')

    local assign = get_item(get_item(parsed, 'id', 'Assign'), 'id', 'VariableIdentifier')
    eq(assign.id, 'VariableIdentifier')
    eq(assign.value, 'this_var')

    neq(var, assign)
  end)

  describe('primitive dicts', function()
    for _, primitive in ipairs({"b", "t", "w"}) do
      it(string.format('Should handle: %s', primitive), function()
        local parsed = token.parsestring(
          grammar,
          make_vim9script(string.format("let %s:glob_var = 1", primitive))
        )

        local primitive_identifier = get_item(parsed, 'id', 'PrimitivesDictIdentifier')
        eq(primitive_identifier.value, primitive)
      end)
    end
  end)
end)
