-- // -- Create the left execution
-- // local __method_call_1 = (function()
-- //   local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { foo })
-- //
-- //   foo = require("vim9script").replace(foo, __vim9_result[1])
-- //   return foo
-- // end)()

-- foo->sort()->reverse()
-- methodcall {
-- left: foo,
-- right: sort, []
-- }

-- reverse(sort(foo))

-- (function()
--   local temp_1 = sort(foo)
--   -- any other mutations here...
--   foo = replace(foo, temp_1)
--
--   local temp_2 = reverse(foo)
--   -- any other mutations here...
--   foo = replace(foo, temp_2)
--
--   return foo
-- end)()
