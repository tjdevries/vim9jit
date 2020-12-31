Vim9__Add = function(a, b)
  local t_a = type(a)
  local t_b = type(b)

  if t_a == 'number' and t_b == 'number' then
    return a + b
  end

  error("Unsupported operation")
end

Vim9__Sub = function(a, b)
  local t_a = type(a)
  local t_b = type(b)

  if t_a == 'number' and t_b == 'number' then
    return a - b
  end

  error("Unsupported operation")
end

Vim9__Mul = function(a, b)
  local t_a = type(a)
  local t_b = type(b)

  if t_a == 'number' and t_b == 'number' then
    return a * b
  end

  error("Unsupported operation")
end

Vim9__Div = function(a, b)
  local t_a = type(a)
  local t_b = type(b)

  if t_a == 'number' and t_b == 'number' then
    return a / b
  end

  error("Unsupported operation")
end
