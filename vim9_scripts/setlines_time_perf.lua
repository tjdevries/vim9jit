--[=[
Original vimscript
vim9script

def VimNew(): number
  var totallen = 0
  for i in range(1, 100000)
    setline(i, '    ' .. getline(i))
    totallen = totallen + len(getline(i))
  endfor
  return totallen
enddef

new
call setline(1, range(100000))
var start = reltime()
echo VimNew()
echo 'Vim new: ' .. reltimestr(reltime(start))
bwipe!

func VimOld()
  let totallen = 0
  for i in range(1, 100000)
    call setline(i, '    ' .. getline(i))
    let totallen += len(getline(i))
  endfor
  return totallen
endfunc

new
call setline(1, range(100000))
start = reltime()
echo VimOld()
echo 'Vim old: ' .. reltimestr(reltime(start))
bwipe!
--]=]

require('vim9jit')

local function VimNew() local totallen = 0
for i = 1, 100000, 1 do
  vim.fn['setline'](i, require('vim9jit').BinaryExpression([[..]], '    ', vim.fn['getline'](i)))totallen = Vim9__Add(totallen, vim.fn['len'](vim.fn['getline'](i)))
end

return totallen
 end
vim.cmd(string.format([[%s%s]], 'new', ""))
vim.fn['setline'](1, vim.fn['range'](100000))
local start = vim.fn['reltime']()
vim.cmd(string.format([[%s%s '%s']], 'echo', "", VimNew()))
