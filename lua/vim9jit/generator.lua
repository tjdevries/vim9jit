local parser = require('vim9jit.parser')
local make_grammar = parser.make_grammar

local tree = require('vim9jit.tree')

local generator = {}

generator.generate = function(str, root)
  local grammar = make_grammar(root)

  local parsed = grammar:match(str)
  if parsed == nil then
    error('Unparsed token: ' .. vim.inspect(str))
  end

  local output = "require('vim9jit')\n"
  for _, v in ipairs(parsed) do
    local g = assert(generator.match[v.id], v.id)
    output = output .. g(v)
  end

  return output, parsed
end



local match = {}

local get_result = function(node)
  if node == nil then return nil end
  if not match[node.id] then error(string.format("Missing: %s", node.id)) end

  return match[node.id](node)
end

local get_value = function(node)
  return node.value
end

match.Expression = function(node)
  local output = ''
  for _, v in ipairs(node) do
    output = output .. get_result(v)
  end

  return output
end

match.Term = function(node)
  local op = node[2]

  local left = node[1]
  local right = node[3]

  -- TODO: handle weird vim semantics
  return string.format("(%s %s %s)",
    get_result(left), get_result(op), get_result(right)
  )
end

match.Factor = match.Term

match.ListLiteral = function(node)
  local results = {}
  for _, v in ipairs(node) do
    table.insert(results, get_result(v))
  end

  return string.format(
    "{ %s }",
    table.concat(results, ", ")
  )
end

match.FuncCall = function(node)
  local func_name = get_result(tree.get_item_with_id(node, 'FuncName'))
  local func_args = get_result(tree.get_item_with_id(node, 'FuncCallArgList'))

  return string.format([[%s(%s)]], func_name, func_args)
end

match.FuncCallArg = match.Expression
match.FuncCallArgList = function(node)
  local output = {}
  for _, v in ipairs(node) do
    table.insert(output, get_result(v))
  end

  return table.concat(output, ", ")
end

match.AnchoredExpression = function(node)
  -- get_result
  -- return '420'
  return get_result(node[1])
end

match.ParenthedExpression = function(node)
  return string.format("(%s)", get_result(node[1]))
end

match.FuncName = function(node)
  local original_func_name = node.value

  if original_func_name == "function" then
    return "vim9jit.vim_function"
  elseif string.match(string.sub(original_func_name, 1, 1), "%l") then
    -- Lowercase functions are always vim functions
    return string.format("vim.fn['%s']", original_func_name)
  else
    return original_func_name
  end
end

match.Number = get_value
match.StringLiteral = get_value

match.Add = function() return "+" end
match.Subtract = function() return "-" end
match.Multiply = function() return "*" end
match.Divide = function() return "/" end

match.Boolean = function(node)
  local val = get_value(node)
  if string.find(val, 'true') then
    return 'true'
  else
    return 'false'
  end
end

generator.match = match

return generator
