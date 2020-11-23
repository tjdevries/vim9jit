
vim9jit = {}

vim9jit.DefaultForType = function(type_str)
  if type_str == 'number' then
    return 0
  else
    error(string.format('Unknown type_str: %s', type_str))
  end
end

local exact_comparisons = {
  ["=="] = true,
  ["==#"] = true,
}

vim9jit.ComparisonEvaluate = function(operator, a, b)
  if exact_comparisons[operator] then
    return a == b
  end

  if operator == "==?" then
    return string.lower(a) == string.lower(b)
  end

  error("Unsupported operator: " .. operator)
end

local expression_operation_dispatch = {}

expression_operation_dispatch["+"] = function(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a + b
  end
end

vim9jit.BinaryExpression = function(operator, a, b)
  if not expression_operation_dispatch[operator] then
    error("Unsupported Binary Expression: " .. operator)
  end

  return expression_operation_dispatch[operator](a, b)
end

-- Binary Op Evaluate
-- Make sure to handle stuff like [1, 2] + [3] -> [1, 2, 3]
vim9jit.BinaryOpEval = function(operator, a, b)
end

vim9jit.tbl = {}
function vim9jit.tbl.filter(tbl, predicate, needs_viml)
  local result
  if type(predicate) ~= 'function' or needs_viml then
    result = vim.fn.filter(tbl, predicate)
  else
    result = vim.tbl_filter(predicate, tbl)
  end

  for k, _ in pairs(tbl) do
    tbl[k] = nil
  end

  for k, v in pairs(result) do
    tbl[k] = v
  end

  return tbl
end

vim9jit.tbl.iter = function(tbl)
  if vim.tbl_islist(tbl) then
    return ipairs(tbl)
  else
    return pairs(tbl)
  end
end

return vim9jit
