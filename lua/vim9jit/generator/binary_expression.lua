
local binary_expression = {}

binary_expression["+"] = [[(function(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a + b
  end

  error("Unsupported operation")
end)(%s, %s)]]

return binary_expression
