local foo = { 5, 3, 1, 2, 4 }
local bar = foo

-- -- foo->sort()->filter((_, y) => y % 2 == 0)
-- (function()
--   local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { foo })
--
--   foo = require("vim9script").replace(foo, __vim9_result[1])
--   foo = __vim9_result[1]
--   return __vim9_result[1]
-- end)()
--
-- foo = (function()
--   local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { foo })
--
--   foo = require("vim9script").replace(foo, __vim9_result[1])
--   return foo
-- end)()
--
-- vim.fn["filter"](foo, function(_, y)
--   return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
-- end)
--
-- P(foo)
-- P(bar)

-- foo->sort()->filter((_, y) => y % 2 == 0)
local __method_call_1 = (function()
  local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { foo })

  foo = require("vim9script").replace(foo, __vim9_result[1])
  return foo
end)()

local __method_call_2 = (function()
  local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("filter", {
    __method_call_1,
    function(_, y)
      return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
    end,
  })

  __method_call_1 = require("vim9script").replace(__method_call_1, __vim9_result[1])
  return __method_call_1
end)()

print "method call 2"
P(__method_call_2)
P(foo)
P(bar)
