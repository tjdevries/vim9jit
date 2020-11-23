
Vim9__Add = function(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a + b
  end

  error("Unsupported operation")
end

Vim9__Sub = function(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a - b
  end

  error("Unsupported operation")
end

Vim9__Mul = function(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a * b
  end

  error("Unsupported operation")
end

Vim9__Div = function(a, b)
  if type(a) == 'number' and type(b) == 'number' then
    return a / b
  end

  error("Unsupported operation")
end
