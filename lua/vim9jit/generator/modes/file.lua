local generator = require('vim9jit.generator')
local gen_utils = require('vim9jit.generator.util')
local indent = require('vim9jit.utils').indent

local get_item_with_id = gen_utils.get_item_with_id
local get_result = gen_utils.get_result
local get_value = gen_utils.get_value
local id_exists = gen_utils.id_exists
local fmt = gen_utils.fmt

local file_mode = {}

local _assignment = function(mode, match, local_prefix)
  local is_global = id_exists(match, 'GlobalVariableIdentifier')

  local identifier = get_result(mode, get_item_with_id(match, 'VariableIdentifier'))
  local expression = get_result(mode, get_item_with_id(match, 'Expression'))
  local type_definition = get_result(mode, get_item_with_id(match, 'TypeDefinition'))

  if generator.strict and local_prefix then
    if type_definition then
      return string.format(
        [[local %s = require('vim9jit').AssertType("%s", %s)%s]],
        identifier,
        type_definition,
        expression,
        "\n"
      )
    end
  end

  local prefix
  if is_global then
    prefix = get_result(mode, get_item_with_id(match, 'GlobalVariableIdentifier'))
  else
    prefix = string.format("%s%s", local_prefix and 'local ' or '', identifier)
  end

  -- This handles things like `let x: number`
  if expression == nil and type_definition then
    expression = string.format([[require('vim9jit').DefaultForType("%s")]], type_definition)
  elseif type_definition == "bool" then
    expression = string.format(
      [[Vim9__Truthy(%s)]],
      expression
    )
  end

  return string.format(
    [[%s = %s%s]],
    prefix,
    expression,
    "\n"
  )
end

file_mode.Assign = function(mode, match)
  return _assignment(mode, match, false)
end

file_mode.Var = function(mode, match)
  return _assignment(mode, match, true)
end

file_mode.Expression = function(mode, match)
  local output = ''
  for _, v in ipairs(match) do
    output = output .. get_result(mode, v)
  end

  return output
end

file_mode.ListLiteral = function(mode, match)
  local results = {}
  for _, v in ipairs(match) do
    table.insert(results, get_result(mode, v))
  end

  return string.format(
    "{ %s }",
    table.concat(results, ", ")
  )
end

-- var x = cond ? foo : bawr
--[[


{ a, b, c}

{
  [1] = { ... cond }, <-- number.
  [2] = { ... foo  },
  [3] = { ... bawr },

  ["id"] = "cond ? foo : bawr"
  ["value"] = asdf
}

--]]
file_mode.ConditionalExpression = function(mode, match)
  return string.format(
    "vim9jit.conditional(%s, function() return %s end, function() return %s end)",
    get_result(mode, match[1]),
    get_result(mode, match[2]),
    get_result(mode, match[3])
  )
end

file_mode.IfBody = file_mode.Expression
file_mode.IfStatement = function(mode, match)
  -- TODO: Rename this to IfExpression
  local if_expression = get_result(mode, match[1])
  local if_body = get_result(mode, get_item_with_id(match, 'IfBody'))

  local else_statement = get_result(mode, get_item_with_id(match, 'ElseStatement'))
  if else_statement then
    else_statement = "\n" .. else_statement
  end

  return string.format(fmt(
    [[
if %s then
%s%s
end
    ]]),
    if_expression, indent(fmt(if_body, false), 2), else_statement
  )
end

file_mode.ElseStatement = function(mode, match)
  local if_body = get_result(mode, get_item_with_id(match, 'IfBody'))

  return string.format("else\n%s", indent(fmt(if_body, false), 2))
end

-- file_mode.ReturnValue = function(mode, match)
-- end
file_mode.ReturnValue = file_mode.Expression
file_mode.Return = function(mode, match)
  return string.format("return %s", get_result(mode, match[1]))
end

file_mode.LambdaLiteral = function(mode, match)
  return string.format(
    "function(%s) return %s end",
    get_result(mode, get_item_with_id(match, "FuncArgList")) or '',
    get_result(mode, get_item_with_id(match, "Expression"))
  )
end

file_mode.FuncName = function(_, match)
  local original_func_name = match.value

  if original_func_name == "function" then
    return "vim9jit.vim_function"
  elseif string.match(string.sub(original_func_name, 1, 1), "%l") then
    -- Lowercase functions are always vim functions
    return string.format("vim.fn['%s']", original_func_name)
  else
    return original_func_name
  end
end

file_mode.FuncCallArg = file_mode.Expression
file_mode.FuncCallArgList = function(mode, match)
  local output = {}
  for _, v in ipairs(match) do
    table.insert(output, get_result(mode, v))
  end

  return table.concat(output, ", ")
end

-- TODO: This may need to handle type things and such...?
file_mode.FuncArg = file_mode.Expression
file_mode.FuncArgList = function(mode, match)
  local output = {}
  for _, v in ipairs(match) do
    table.insert(output, get_result(mode, v))
  end

  return table.concat(output, ", ")
end

file_mode.FuncCall = function(mode, match)
  local func_name = get_result(mode, get_item_with_id(match, 'FuncName'))
  local func_args = get_result(mode, get_item_with_id(match, 'FuncCallArgList'))

  return string.format([[%s(%s)]], func_name, func_args)
end

file_mode.MethodCall = function(mode, match)
  return require('vim9jit.generator.method_call')(match)
end

file_mode.For = function(mode, match)
  local for_var = get_result(mode, get_item_with_id(match, 'ForVar'))
  local for_body = get_result(mode, get_item_with_id(match, 'ForBody'))
  local formatted_for_body = indent(trim(for_body), 2)

  local for_obj_match = get_item_with_id(match, 'ForObj')
  local for_obj_func_name_match = get_item_with_id(for_obj_match, 'FuncName', true) or {}

  if for_obj_func_name_match.value == 'range' then
    local func_call_args = get_item_with_id(for_obj_match, 'FuncCallArgList', true)

    local range_expr
    local range_max
    local range_step
    if #func_call_args == 1 then
      range_expr = 1
      range_max = get_result(mode, func_call_args[1])
      range_step = 1
    elseif #func_call_args == 2 then
      range_expr = get_result(mode, func_call_args[1])
      range_max = get_result(mode, func_call_args[2])
      range_step = 1
    else
      error("Unsupported range style")
    end


    if range_max == nil then
      range_max = range_expr
      range_expr = 1
    end

    return string.format(fmt(
      [[
for %s = %s, %s, %s do
%s
end
      ]]), for_var, range_expr, range_max, range_step, formatted_for_body
    )
  else
    local for_obj = get_result(mode, for_obj_match)

    return string.format(fmt(
      [[
for _, %s in %s do
%s
end
      ]]), for_var, for_obj, formatted_for_body
    )
  end
end

file_mode.ForObj = function(mode, match)
  assert(#match == 1, "ForObj can only have one object here.")
  return string.format("require('vim9jit').tbl.iter(%s)", get_result(mode, match[1]))
end

file_mode.CommandName = get_value
file_mode.CommandBang = function(mode, match) return '"!"' end
file_mode.CommandArguments = file_mode.Expression
file_mode.Command = function(mode, match)
  local command_name = get_result(mode, get_item_with_id(match, 'CommandName'))
  local command_bang = get_result(mode, get_item_with_id(match, 'CommandBang')) or '""'
  local command_arguments = get_result(mode, get_item_with_id(match, 'CommandArguments'))

  if command_arguments then
    return string.format(
      "vim.cmd(string.format(%s, '%s', %s, %s))\n",
      "[[%s%s '%s']]",
      command_name,
      command_bang,
      command_arguments
    )
  else
    return string.format(
      "vim.cmd(string.format(%s, '%s', %s))\n",
      "[[%s%s]]",
      command_name,
      command_bang
    )
  end
end

file_mode.UnparsedCapturedError = function(mode, match)
  return inspect(match)
end

file_mode.Boolean = function(mode, match)
  local value = match.value

  if string.find(value, "true") then
    return "true"
  else
    return "false"
  end
end

file_mode.ComparisonExpression = function(mode, match)
  -- Operator, value, value
  return string.format(
    "require('vim9jit').ComparisonEvaluate([[%s]], %s, %s)",
    get_result(mode, match[2]),
    get_result(mode, match[1]),
    get_result(mode, match[3])
  )
end

local optimized_binop = {}

optimized_binop["+"] = "Vim9__Add(%s, %s)"
optimized_binop["-"] = "Vim9__Sub(%s, %s)"
optimized_binop["*"] = "Vim9__Mul(%s, %s)"
optimized_binop["/"] = "Vim9__Div(%s, %s)"


-- TODO: Some of these could be short circuited and just call "bool" on them.
file_mode.BinaryExpression = function(mode, match)
  local operator = get_result(mode, match[2])
  local format_str = optimized_binop[operator]
  if not format_str then
    format_str = "require('vim9jit').BinaryExpression([[" .. operator ..  "]], %s, %s)"
  end

  return string.format(
    format_str,
    get_result(mode, match[1]),
    get_result(mode, match[3])
  )
end

file_mode.BinaryOperator = get_value

file_mode.Comment = function(mode, match)
  return string.format("-- %s", match.value)
end

file_mode.VariableIdentifier = get_value

local _dict_value = function(accessor)
  return function(mode, match)
    return string.format("%s['%s']", accessor, get_result(mode, match[1]))
  end
end
file_mode.GlobalVariableIdentifier = _dict_value('vim.g')
file_mode.VimVariableIdentifier = _dict_value('vim.v')

file_mode.FuncBody = file_mode.Expression
file_mode.ForBody = file_mode.Expression

file_mode.FuncStatement = function(mode, match)
  -- TODO: 
  --    - There are names of functions in vimscript
  --        that are NOT valid identifiers to be used directly in Lua.
  --        We probably need to do something like
  --        _ENV["func_name"] = function() ... end
  --    - Probably want to expose these as s:local functions for vimscript land
  --        so that people can use the script hax they are used to.
  return string.format([[local function %s(%s) %s end]],
    get_result(mode, get_item_with_id(match, "FuncName")),
    get_result(mode, get_item_with_id(match, "FuncArgList")) or '',
    get_result(mode, get_item_with_id(match, "FuncBody"))
  )
end

file_mode.StringLiteral = get_value
file_mode.ForVar = get_value
file_mode.TypeDefinition = get_value
file_mode.StringOperator = get_value
file_mode.Number = get_value
file_mode.CapturedEOL = get_value
file_mode.ComparisonExpressionOperator  = get_value

return file_mode
