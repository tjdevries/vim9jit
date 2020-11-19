local generator = require('vim9jit.generator')
local util = require('vim9jit.generator.util')

local get_item = util.get_item
local get_item_with_id = util.get_item_with_id
local id_exists = util.id_exists


local get_result = function(match)
  return util.get_result(generator, match)
end

local func_node_overrides = {}

func_node_overrides['add'] = function(obj, match)
  local func_args = get_result(get_item_with_id(match, 'FuncCallArgList'))

  return string.format(
    "(function() local __MethodCallAdd = %s; table.insert(__MethodCallAdd, %s); return __MethodCallAdd end)()",
    obj, func_args
  )
end

local func_node_default = function(obj, match)
  local func_node = get_result(get_item_with_id(match, 'FuncName'))
  local func_args = get_result(get_item_with_id(match, 'FuncCallArgList'))

  return string.format("%s(%s, %s)", func_node, obj, func_args)
end

return function(match)
  local obj = get_result(match[1])

  local result = "(function()\n"
  result = result .. string.format("local __TSMethodCall1 = %s\n", obj)


  for i = 2, #match do
    local func_node = get_item_with_id(match[i], 'FuncName')
    local assigner = func_node_overrides[func_node.value] or func_node_default

    local assigner_obj = string.format("__TSMethodCall%s", i - 1)
    local assigner_text = assigner(assigner_obj, match[i])
    if not assigner_text then
      assigner_text = func_node_default(assigner_obj, match[i])
    end

    result = result .. string.format(
      "local __TSMethodCall%s = %s\n",
      i, assigner_text)
  end

  result = result .. string.format("return __TSMethodCall%s\n", #match)
  result = result .. "end)()"

  if true then return result end

  -- TODO: handle moving different position...
  -- Example of special casing a method call to do the right thing.
  if func_node.value == "add" then
  end
end

