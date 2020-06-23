--[=[
Original vimscript
vim9script

def VimNew(): number
  let totallen = 0
  for i in range(1, 100000)
    setline(i, '    ' .. getline(i))
    totallen = totallen + len(getline(i))
  endfor
  return totallen
enddef

new
call setline(1, range(100000))
let start = reltime()
echo VimNew()
echo 'Vim new: ' .. reltimestr(reltime(start))
bwipe!

--]=]


local function VimNew()
  local totallen = 0
  for i = 1, 100000, 1 do
    vim.fn['setline'](i, '    ' .. vim.fn['getline'](i))
    totallen = totallen + vim.fn['len'](vim.fn['getline'](i))
  end

  return totallen
end

vim.cmd(string.format([[%s%s]], 'new', ""))
vim.fn['setline'](1, vim.fn['range'](100000))
local start = vim.fn['reltime']()
vim.cmd(string.format([[%s%s '%s']], 'echo', "", VimNew()))
vim.cmd(string.format([[%s%s '%s']], 'echo', "", 'Vim new: ' .. vim.fn['reltimestr'](vim.fn['reltime'](start))))
vim.cmd(string.format([[%s%s]], 'bwipe', "!"))

