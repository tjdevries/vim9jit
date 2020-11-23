require('vim9jit.runtime.operations')

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
