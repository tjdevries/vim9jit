RELOAD('vim9jit')

local generator = require('vim9jit.generator')

local eval = function(s, force)
  local generated, parsed = generator.generate(s, 'Expression')
  local inner = vim.split(generated, "\n")
  inner[#inner] = "return " .. inner[#inner]

  local to_load = string.format([[
    return (function()
      %s
    end)()
  ]], table.concat(inner, "\n"))

  print("Executing... ", to_load)

  local to_call = loadstring(to_load)
  if not to_call or force then
    error(string.format("Failed: %s\n\n%s\n", to_load, vim.inspect(parsed)))
    return
  end

  return to_call()
end


print("eval: ", eval('5 + 5 + 5'))
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("eval: ", eval('(2 + 2) + (3 * 5)'))
