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


local start = vim.fn['reltime']()

-- local Vim9JitAdd = function(a, b)

local function VimNew() local sum = 0
for i = 1, 2999999, 1 do
  sum = (function(a, b)
    if type(a) == 'number' and type(b) == 'number' then
      return a + b
    end

    error("Unsupported operation")
  end)(sum, i)

  sum = sum + 1
end


return sum
 end
vim.cmd(string.format([[%s%s '%s']], 'echo', "", VimNew()))
vim.cmd(string.format([[%s%s '%s']], 'echo', "", vim.fn['reltimestr'](vim.fn['reltime'](start))))

