-- var x = [3, 2, 1]
local x = { 3, 2, 1 }
-- var y = sort(x)
local y = (function()
  -- TODO:
  local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { x })
  x = __vim9_result[2][1]
  return __vim9_result[1]
end)()

-- echo y
print("y:", vim.inspect(y))

-- echo x
print("x:", vim.inspect(x))

-- x->sort()->filter((_, y) => y % 2 == 0)
-- filter(sort(x), function(_, y) return y % 2 == 0 end)

-- x->sort()
-- x->filter()
-- x->sort()->filter(...)

-- two types of mutation
--  Argument->Return value mutation
--  Argument-only mutation

local example = function(x)
  return vim.deepcopy(x)
end

local bar = { 1, 2, 3 }
local foo = example(bar)
foo[1] = false

print "bar:"
P(bar)
print "foo:"
P(foo)

-- (function()
--   local res_1 = (function()
--     local res_1 = vim.fn.sort(x)
--     x = res_1[2][1]
--     return res_1[1]
--   end)()
--
--   local res_2 = vim.fn.filter(res_1, function() end)
--   x = res_2[2][1]
--
--   return x
-- end)()
