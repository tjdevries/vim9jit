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

