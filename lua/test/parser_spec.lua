require('plenary.test_harness'):setup_busted()

local grammar = require('vim9jit.parser').grammar
local token = require('vim9jit.token')

local helpers = require('test.helpers')
local eq, neq, get_item = helpers.eq, helpers.neq, helpers.get_item


local trim = function (s) return s:match('^%s*(.*%S)') or '' end

local make_vim9script = function(text)
  return 'vim9script\n' .. helpers.dedent(text)
end

local get_parsed = function(s)
  return token.parsestring(grammar, make_vim9script(s))
end

describe('parser', function()
  describe('Var', function()
    it('should load vim9script file', function()
      local parsed = token.parsestring(grammar, "vim9script")

      eq(parsed.id, 'vim9script')
    end)

    it('should parse simple Var statements', function()
      local parsed = token.parsestring(grammar, make_vim9script("var x = 1"))
      neq(nil, parsed)

      local var = get_item(parsed, 'id', 'Var')

      eq(var.id, 'Var')
    end)

    it('should parse hex numbers', function()
      eq(
        '0x1234DEADBEEF',
        get_item(get_parsed('var x = 0x1234DEADBEEF'), 'id', 'Number').value
      )
    end)

    it('should parse const', function()
      eq(
        '"is constant"',
        get_item(get_parsed('const x = "is constant"'), 'id', 'String').value
      )
    end)

    it('should parse final', function()
      eq(
        '"is final"',
        get_item(get_parsed('const x = "is final"'), 'id', 'String').value
      )
    end)

    it('should parse simple addition statements', function()
      local parsed = token.parsestring(grammar, make_vim9script("var x = 1 + 2"))
      neq(nil, parsed)

      local var = get_item(parsed, 'id', 'Var')

      eq(var.id, 'Var')
      eq(var.value, 'var x = 1 + 2')
    end)

    it('should parse early declaration', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        var x
        if true
          x = 1
        else
          x = 2
        endif
      ]])

      neq(nil, parsed)

      local var = get_item(parsed, 'id', 'Var')

      eq(var.id, 'Var')
      eq(trim(var.value), 'var x')
    end)

    describe('TypeDefinition', function()
      it('should parse simple type definitions', function()
        local parsed = token.parsestring(grammar, make_vim9script("var x: number = 1 + 2"))
        neq(nil, parsed)

        local var = get_item(parsed, 'id', 'Var')

        eq(var.id, 'Var')
        eq(var.value, 'var x: number = 1 + 2')

        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        neq(nil, type_definition)
        eq(type_definition.value, 'number')
      end)

      it('should allow for type definition with no assign', function()
        local parsed = token.parsestring(grammar, make_vim9script("var x: number"))
        neq(nil, parsed)

        local var = get_item(parsed, 'id', 'Var')

        eq(var.id, 'Var')
        eq(var.value, 'var x: number')

        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        neq(nil, type_definition)
        eq(type_definition.value, 'number')
      end)

      it('should handle list types', function()
        local parsed = get_parsed("var x: list<number>")
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'list<number>')
      end)

      it('should handle nested list types', function()
        local parsed = get_parsed("var x: list<list<bool>>")
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'list<list<bool>>')
      end)

      it('should handle function types', function()
        local parsed = get_parsed('var x: func: string')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func: string')
      end)

      it('should handle function signature types', function()
        local parsed = get_parsed('var x: func(number): number')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func(number): number')
      end)

      it('should handle function signature with multiple args', function()
        local parsed = get_parsed('var x: func(number, string): number')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func(number, string): number')
      end)

      it('should handle function with args and complex return', function()
        local parsed = get_parsed('var x: func(number, string): list<number>')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func(number, string): list<number>')
      end)

      it('should handle function with only ellipsis', function()
        local parsed = get_parsed('var x: func(...): number')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func(...): number')
      end)

      it('should handle function with ellipsis', function()
        local parsed = get_parsed('var x: func(number, ...): number')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func(number, ...): number')
      end)

      it('should handle union types', function()
        local parsed = get_parsed('var x: string|number')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'string|number')
      end)

      it('should handle function with union types', function()
        local parsed = get_parsed('var x: func(number|bool, ...): number|string')
        local type_definition = get_item(parsed, 'id', 'TypeDefinition')
        eq(type_definition.value, 'func(number|bool, ...): number|string')
      end)
    end)

    it('should parse handle global variables', function()
      local parsed = token.parsestring(grammar, make_vim9script("var g:glob_var = 1"))
      neq(nil, parsed)

      local var = get_item(parsed, 'id', 'Var')
      eq(var.id, 'Var')

      local global_var = get_item(var, 'id', 'GlobalVariableIdentifier')
      neq(nil, global_var)
      eq(global_var.value, 'g:glob_var')

      local var = get_item(global_var, 'id', 'VariableIdentifier')
      eq(var.value, 'glob_var')
    end)

    it('should handle Z', function()
      local parsed = get_item(get_parsed [[var Z = g:cond ? FuncOne : FuncTwo]], 'id', 'Var')
      eq([[var Z = g:cond ? FuncOne : FuncTwo]], parsed.value)
    end)

    it('should allow updating an existing variable', function()
      local parsed = token.parsestring(grammar, make_vim9script([[
        var this_var: number = 1
        this_var = 3
      ]]))
      neq(nil, parsed)

      local var = get_item(get_item(parsed, 'id', 'Var'), 'id', 'VariableIdentifier')
      eq(var.id, 'VariableIdentifier')
      eq(var.value, 'this_var')

      local assign = get_item(get_item(parsed, 'id', 'Assign'), 'id', 'VariableIdentifier')
      eq(assign.id, 'VariableIdentifier')
      eq(assign.value, 'this_var')

      neq(var, assign)
    end)

    describe('with function calls', function()
      it('should allow setting variables to function calls', function()
        local parsed = token.parsestring(grammar, make_vim9script("var range_func = range(1, 10)"))
        neq(nil, parsed)

        local func_call = get_item(parsed, 'id', 'FuncCall')
        neq(nil, func_call)

        eq('range', get_item(func_call, 'id', 'FuncName').value)
      end)

      it('should allow setting variables to function calls with no variables', function()
        local parsed = token.parsestring(grammar, make_vim9script("var cur_pos = getcurpos()"))
        neq(nil, parsed)

        local func_call = get_item(parsed, 'id', 'FuncCall')
        neq(nil, func_call)

        eq('getcurpos', get_item(func_call, 'id', 'FuncName').value)
      end)
    end)

    it('should allow strings', function()
      local parsed = token.parsestring(grammar, make_vim9script [[var x = "hello world"]])
      neq(nil, parsed)

      local s = get_item(parsed, 'id', 'String')
      neq(nil, s)
    end)

    it('should allow string concat', function()
      local parsed = token.parsestring(grammar, make_vim9script [[var x = "hello" .. ' world']])
      neq(nil, parsed)

      local exp = get_item(parsed, 'id', 'Expression')
      neq(nil, exp)

      local s = get_item(parsed, 'id', 'String', 1)
      neq('hello', s.value)

      local s_2 = get_item(parsed, 'id', 'String', 2)
      neq([[ world]], s_2.value)
    end)

    describe('ListExpression', function()
      it('should allow empty list', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          var x = []
        ]])

        eq('[]', get_item(parsed, 'id', 'ListExpression').value)
      end)

      it('should allow lists with 1 item', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          var x = [1]
        ]])

        eq('[1]', get_item(parsed, 'id', 'ListExpression').value)
        eq('1', get_item(parsed, 'id', 'Number').value)
      end)

      it('should allow multiple items', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          var x = [1, a, true]
        ]])

        local list_expression = get_item(parsed, 'id', 'ListExpression')
        eq('[1, a, true]', list_expression.value)
        eq('1', get_item(list_expression, 'id', 'Number').value)
        eq('a', get_item(list_expression, 'id', 'VariableIdentifier').value)
        eq('true', get_item(list_expression, 'id', 'Boolean').value)
      end)

      it('should allow nested lists', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          var x = [ [1, 2], [3, 4] ]
        ]])

        local list_expression = get_item(parsed, 'id', 'ListExpression')
        eq('[ [1, 2], [3, 4] ]', list_expression.value)
      end)
    end)

    it('should handle empty dictionaries', function()
      local parsed = get_parsed("var x = {}")
      neq(nil, get_item(parsed, 'id', 'DictExpression'))
    end)

    it('should handle dictionaries with a single key', function()
      local parsed = get_parsed("var x = {'a': 1}")

      local dict = get_item(parsed, 'id', 'DictExpression')
      eq("'a'", get_item(dict, 'id', 'DictKey').value)
      eq("1", get_item(dict, 'id', 'DictValue').value)
    end)

    it('should handle dictionaries with a multiple keys', function()
      local parsed = get_parsed("var x = {'a': 1, 'b': 2, 'c': v:true}")

      local dict = get_item(parsed, 'id', 'DictExpression')
      eq("'a'", get_item(dict, 'id', 'DictKey').value)
      eq("1", get_item(dict, 'id', 'DictValue').value)

      eq("'b'", get_item(dict, 'id', 'DictKey', 2).value)
      eq("2", get_item(dict, 'id', 'DictValue', 2).value)

      eq("'c'", get_item(dict, 'id', 'DictKey', 3).value)
      eq("v:true", get_item(dict, 'id', 'DictValue', 3).value)
    end)
  end)

  describe('primitive dicts', function()
    for _, primitive in ipairs({"b", "t", "w"}) do
      it(string.format('Should handle: %s', primitive), function()
        local parsed = token.parsestring(
          grammar,
          make_vim9script(string.format("var %s:glob_var = 1", primitive))
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

    it('should allow argument definitions', function()
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

    it('should allow comments in functions', function()
      local parsed = token.parsestring(grammar, make_vim9script([[
        def MyFunc(arg)
          assert_equal(1, 1)
          # Hello world
        enddef
      ]]))

      neq(nil, parsed)

      eq("# Hello world", trim(get_item(parsed, 'id', 'Comment').value))
    end)

    it('should allow function defs with types', function()
      local parsed = get_parsed [[
        def FuncOne(arg: number): string
          return 'yes'
        enddef
      ]]

      local arg_defintion = get_item(parsed, 'id', 'FuncArgList')

      neq(nil, arg_defintion)

      eq('arg', get_item(arg_defintion, 'id', 'VariableIdentifier').value)
      eq('number', get_item(arg_defintion, 'id', 'TypeDefinition').value)
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
          var sum = 1

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

        eq('sum = sum + 1', trim(get_item(parsed, 'id', 'ForBody').value))
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
          var sum = 0
          for i in range(1, 100)
            sum = sum + 1
          endfor
        enddef
      ]])

      neq(nil, parsed)

      local func_def = get_item(parsed, 'id', 'FuncDef')
      neq(nil, func_def)
      eq('VimNew', get_item(func_def, 'id', 'FuncName').value)

      local func_body = get_item(parsed, 'id', 'FuncBody')
      neq(nil, func_body)

      local func_body_loop = get_item(func_body, 'id', 'For')
      neq(nil, func_body_loop)
    end)
  end)

  describe('primitives', function()
    it('should know about true', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        var x = true
      ]])

      neq(nil, parsed)

      local boolean = get_item(parsed, 'id', 'Boolean')
      neq(nil, boolean)
      eq('true', boolean.value)
    end)

    it('should know about false', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        var x = false
      ]])

      neq(nil, parsed)

      local boolean = get_item(parsed, 'id', 'Boolean')
      neq(nil, boolean)
      eq('false', boolean.value)
    end)

    it('should know about v:true', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        var x = v:true
      ]])

      neq(nil, parsed)

      local boolean = get_item(parsed, 'id', 'Boolean')
      neq(nil, boolean)
      eq('v:true', boolean.value)
    end)

    it('should know about v:false', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        var x = v:false
      ]])

      neq(nil, parsed)

      local boolean = get_item(parsed, 'id', 'Boolean')
      neq(nil, boolean)
      eq('v:false', boolean.value)
    end)
  end)

  describe('Comments', function()
    it('should work on a line by itself', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        # This is a comment
      ]])

      neq(nil, parsed)

      local comment = get_item(parsed, 'id', 'Comment')
      neq(nil, comment)
      eq("# This is a comment", trim(comment.value))
    end)
  end)

  describe('Expressions', function()
    describe('function calls', function()
      it('should support functions without `call`', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          def Test_expr1()
            assert_equal(1, 1)
          enddef
        ]])

        local func_body = get_item(parsed, 'id', 'FuncBody')
        neq(nil, func_body)

        eq("assert_equal", get_item(func_body, 'id', 'FuncName').value)
      end)
    end)

    describe('conditionals', function()
      it('should support simple ? usage', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          var x = 1 ? 2 : 3
        ]])

        neq(nil, parsed)

        local conditional = get_item(parsed, 'id', 'ConditionalExpression')
        neq(nil, conditional)

        eq("1 ? 2 : 3", conditional.value)
      end)

      it('should work in functions', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          def Test_expr1()
            assert_equal("one", 1 ? "one" : "two")
          enddef
        ]])

        local func_body = get_item(parsed, 'id', 'FuncBody')
        neq(nil, func_body)

        eq("assert_equal", get_item(func_body, 'id', 'FuncName').value)
      end)

      it('should work in functions on multiple lines', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          def Test_expr1()
            assert_equal("one", 1 ?
              "one" :
              "two")
          enddef
        ]])

        local func_body = get_item(parsed, 'id', 'FuncBody')
        neq(nil, func_body)

        eq("assert_equal", get_item(func_body, 'id', 'FuncName').value)
      end)

      it('should work in functions on multiple lines, ?', function()
        local parsed = token.parsestring(grammar, make_vim9script [[
          def Test_expr1()
            assert_equal("one", 1
                ?
              "one" :
              "two")
          enddef
        ]])

        local func_body = get_item(parsed, 'id', 'FuncBody')
        neq(nil, func_body)

        eq("assert_equal", get_item(func_body, 'id', 'FuncName').value)
      end)

    end)
  end)

  describe('IfStatement', function()
    it("should handle simple expressions", function()
      local parsed = token.parsestring(grammar, make_vim9script [[
        if x
          assert_equal(1, 1)
        endif
       ]])

       neq(nil, parsed)

       local if_statement = get_item(parsed, 'id', 'IfStatement')
       neq(nil, if_statement)

       eq("assert_equal", get_item(if_statement, 'id', 'FuncName').value)
    end)

    it("should handle if else expression", function()
      local contents = make_vim9script [[
        if cond
          assert_equal(1, 1)
        else
          assert_equal(1, 2)
        endif
      ]]

      local parsed = token.parsestring(grammar, contents)

      neq(nil, parsed)

      local if_statement = get_item(parsed, 'id', 'IfStatement')
      neq(nil, if_statement)

      eq("assert_equal", get_item(if_statement, 'id', 'FuncName').value)

      local else_statement = get_item(parsed, 'id', 'ElseStatement')
      neq(nil, else_statement)
    end)
  end)

  describe("MethodCall", function()
    it("should handle simple case", function()
      local parsed = token.parsestring(grammar, make_vim9script("var x = myList->add(1)"))
      neq(nil, parsed)

      local method_call = get_item(parsed, 'id', 'MethodCall')
      neq(nil, method_call)

      eq("add", get_item(method_call, 'id', 'FuncName').value)
    end)
  end)



  -- {{{ Example Tests
  describe('vim9testdir', function()
    it('should parse', function()
      local parsed = token.parsestring(grammar, make_vim9script [[
def Test_expr1()
  assert_equal('one', true ? 'one' : 'two')
  assert_equal('one', 1 ?
			'one' :
			'two')
  assert_equal('one', 'x' ? 'one' : 'two')
  assert_equal('one', 'x'
  			? 'one'
			: 'two')

  var var = 1
  assert_equal('one', var ? 'one' : 'two')

  assert_equal('two', false ? 'one' : 'two')
  assert_equal('two', 0 ? 'one' : 'two')

  assert_equal('two', '' ? 'one' : 'two')
  var = 0
  assert_equal('two', var ? 'one' : 'two')

  # Not supported yet
  #
  # if has('float')
  #   assert_equal('one', 0.1 ? 'one' : 'two')
  # endif
  # if has('float')
  #   assert_equal('two', 0.0 ? 'one' : 'two')
  # endif
  # assert_equal('one', [0] ? 'one' : 'two')
  # assert_equal('one', 0z1234 ? 'one' : 'two')
  # assert_equal('one', #{x: 0} ? 'one' : 'two')
  # assert_equal('two', 0z ? 'one' : 'two')
  # assert_equal('two', [] ? 'one' : 'two')
  # assert_equal('two', {} ? 'one' : 'two')

  # var Some: func = function('len')
  # var Other: func = function('winnr')
  # var Res: func = g:atrue ? Some : Other
  # assert_equal(function('len'), Res)

  # var RetOne: func(string): number = function('len')
  # var RetTwo: func(string): number = function('winnr')
  # var RetThat: func = g:atrue ? RetOne : RetTwo
  # assert_equal(function('len'), RetThat)

  # var X = FuncOne
  # var Y = FuncTwo
  # var Z = g:cond ? FuncOne : FuncTwo
  # assert_equal(123, Z(3))
enddef
]])

      neq(nil, parsed)

      neq(nil, get_item(parsed, 'id', 'FuncDef'))
    end)
  end)
  -- }}}
end)
