--[=[
Original vimscript
vim9script

let start = reltime()

def VimNew(): number
  let sum = 0
  for i in range(1, 2999999)
    sum = sum + i
  endfor

  return sum
enddef

echo VimNew()
echo reltimestr(reltime(start))

--]=]


local start = vim.fn['reltime']()

local function VimNew()
  local sum = 0
  for i = 1, 2999999, 1 do
    sum = sum + i
  end


  return sum
end

vim.cmd(string.format([[%s%s '%s']], 'echo', "", VimNew()))
vim.cmd(string.format([[%s%s '%s']], 'echo', "", vim.fn['reltimestr'](vim.fn['reltime'](start))))

