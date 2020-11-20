
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

-- Binary Op Evaluate
-- Make sure to handle stuff like [1, 2] + [3] -> [1, 2, 3]
vim9jit.BinaryOpEval = function(operator, a, b)
end

return vim9jit
