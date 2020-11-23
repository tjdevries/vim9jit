--[=[
Original vimscript
vim9script

var start = reltime()

def VimNew(): number
  var sum = 0
  for i in range(1, 2999999)
    sum = sum + i
  endfor

  return sum
enddef

echo VimNew()
echo reltimestr(reltime(start))

--]=]

require('vim9jit')

local start = vim.fn['reltime']()

local function VimNew() local sum = 0
for i = 1, 2999999, 1 do
  sum = Vim9__Add(sum, i)
end


return sum
 end
vim.cmd(string.format([[%s%s '%s']], 'echo', "", VimNew()))
vim.cmd(string.format([[%s%s '%s']], 'echo', "", vim.fn['reltimestr'](vim.fn['reltime'](start))))

