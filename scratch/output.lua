local start = vim.fn["reltime"]()

local VimNew = function()
  local sum = 1

  -- for _, i in ipairs(vim.fn["range"](1, 2999999)) do
  for i = 1, 2999999 do
    sum = (sum + i)
  end

  return sum
end

local sum = (VimNew)()
P(vim.fn.reltimestr(vim.fn["reltime"](start)))
