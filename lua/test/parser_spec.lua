require('plenary.test_harness'):setup_busted()

local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local helpers = require('test.helpers')
local eq, neq, get_item = helpers.eq, helpers.neq, helpers.get_item


local make_vim9script = function(text)
  return 'vim9script\n' .. helpers.dedent(text)
end

describe('parser', function()
  describe('Let', function()
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

    describe('with function calls', function()
      it('should allow setting variables to function calls', function()
        local parsed = token.parsestring(grammar, make_vim9script("let range_func = range(1, 10)"))
        neq(nil, parsed)

        local func_call = get_item(parsed, 'id', 'FuncCall')
        neq(nil, func_call)

        eq('range', get_item(func_call, 'id', 'FuncName').value)
      end)

      it('should allow setting variables to function calls with no variables', function()
        local parsed = token.parsestring(grammar, make_vim9script("let cur_pos = getcurpos()"))
        neq(nil, parsed)

        local func_call = get_item(parsed, 'id', 'FuncCall')
        neq(nil, func_call)

        eq('getcurpos', get_item(func_call, 'id', 'FuncName').value)
      end)
    end)

    it('should allow strings', function()
      local parsed = token.parsestring(grammar, make_vim9script [[let x = "hello world"]])
      neq(nil, parsed)

      local s = get_item(parsed, 'id', 'String')
      neq(nil, s)
    end)

    it('should allow string concat', function()
      local parsed = token.parsestring(grammar, make_vim9script [[let x = "hello" .. ' world']])
      neq(nil, parsed)

      local exp = get_item(parsed, 'id', 'Expression')
      neq(nil, exp)

      local s = get_item(parsed, 'id', 'String', 1)
      neq('hello', s.value)

      local s_2 = get_item(parsed, 'id', 'String', 2)
      neq([[ world]], s_2.value)
    end)
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

  describe('function definitions', function()
    it('should allow empty definitions', function()
      local parsed = token.parsestring(grammar, make_vim9script([[
        def Concat()
        enddef
      ]]))
      neq(nil, parsed)

      local func_def = get_item(parsed, 'id', 'FuncDef')
      neq(nil, func_def)
      eq('Concat', get_item(func_def, 'id', 'FuncName').value)
    end)

    it('shouold allow argument definitions', function()
      local parsed = token.parsestring(grammar, make_vim9script([[
        def Concat(arg)
        enddef
      ]]))
      neq(nil, parsed)

      local func_def = get_item(parsed, 'id', 'FuncDef')
      neq(nil, func_def)
      eq('Concat', get_item(func_def, 'id', 'FuncName').value)


      eq('arg', get_item(func_def, 'id', 'FuncArgList').value)
    end)

    it('should allow return statements', function()
      local parsed = token.parsestring(grammar, make_vim9script([[
        def ReturnsPlusOne(arg)
          return arg + 1
        enddef
      ]]))

      neq(nil, parsed)

      local func_def = get_item(parsed, 'id', 'FuncDef')
      neq(nil, func_def)
      eq('ReturnsPlusOne', get_item(func_def, 'id', 'FuncName').value)


      eq('arg', get_item(func_def, 'id', 'FuncArgList').value)

      eq('arg + 1', get_item(func_def, 'id', 'ReturnValue').value)
    end)
  end)

  describe('for loops', function()
    it('parse empty for loops', function()
      local parsed = token.parsestring(grammar, make_vim9script([[
        for i in my_list
        endfor
      ]]))

      neq(nil, parsed)

      local for_loop = get_item(parsed, 'id', 'For')
      neq(nil, for_loop)

      local for_var = get_item(for_loop, 'id', 'ForVar')
      neq(nil, for_var)
      eq('i', for_var.value)

      local for_obj = get_item(for_loop, 'id', 'ForObj')
      neq(nil, for_obj)
      eq('my_list', for_obj.value)
    end)

    it('parse loops with function calls', function()
        local parsed = token.parsestring(grammar, make_vim9script([[
          for i in range(1, 100)
          endfor
        ]]))

        neq(nil, parsed)

        local for_loop = get_item(parsed, 'id', 'For')
        neq(nil, for_loop)

        local for_var = get_item(for_loop, 'id', 'ForVar')
        neq(nil, for_var)
        eq('i', for_var.value)

        local for_obj = get_item(for_loop, 'id', 'ForObj')
        neq(nil, for_obj)
        eq('range(1, 100)', for_obj.value)
    end)

    it('parse for loops with something inside', function()
        local parsed = token.parsestring(grammar, make_vim9script([[
          let sum = 1

          for i in range(1, 100)
            sum = sum + 1
          endfor
        ]]))

        neq(nil, parsed)

        local for_loop = get_item(parsed, 'id', 'For')
        neq(nil, for_loop)

        local for_var = get_item(for_loop, 'id', 'ForVar')
        neq(nil, for_var)
        eq('i', for_var.value)

        local for_obj = get_item(for_loop, 'id', 'ForObj')
        neq(nil, for_obj)
        eq('range(1, 100)', for_obj.value)

        eq('sum = sum + 1', vim.trim(get_item(parsed, 'id', 'ForBody').value))
    end)
  end)

  describe('command', function()
    it('Works for echo', function()
      local parsed = token.parsestring(grammar, make_vim9script [[echo my_func()]])
      neq(nil, parsed)

      local command = get_item(parsed, 'id', 'Command')
      neq(nil, command)

      local command_name = get_item(command, 'id', 'CommandName')
      eq('echo', command_name.value)

      local command_argument = get_item(command, 'id', 'CommandArguments')
      eq('my_func()', command_argument.value)
    end)

    it('Works for complicated echo', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        echo 'Vim new: ' .. reltimestr(reltime(start))
      ]])
      neq(nil, parsed)

      local command = get_item(parsed, 'id', 'Command')
      neq(nil, command)

      local command_name = get_item(command, 'id', 'CommandName')
      eq('echo', command_name.value)

      local command_argument = get_item(command, 'id', 'CommandArguments')
      eq("'Vim new: ' .. reltimestr(reltime(start))", command_argument.value)
    end)
  end)

  describe('larger constructs', function()
    it('Can have defitions, functions and loops together', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        def VimNew()
          let sum = 0
          for i in range(1, 100)
            sum = sum + 1
          endfor
        enddef
      ]])

      neq('parsed', parsed)

      local func_def = get_item(parsed, 'id', 'FuncDef')
      neq(nil, func_def)
      eq('VimNew', get_item(func_def, 'id', 'FuncName').value)

      local func_body = get_item(parsed, 'id', 'FuncBody')
      neq(nil, func_body)

      local func_body_loop = get_item(func_body, 'id', 'For')
      neq(nil, func_body_loop)
    end)
  end)
end)
