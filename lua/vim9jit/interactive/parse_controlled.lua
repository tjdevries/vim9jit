RELOAD('vim9jit')

local thread = coroutine.create(function()
  local x = 1
  local y = 2
  print(x, y)
  return 5
end)

debug.sethook(print, 'crl', 10)

local res = coroutine.resume(thread)
print("RES:", res)

debug.sethook()
