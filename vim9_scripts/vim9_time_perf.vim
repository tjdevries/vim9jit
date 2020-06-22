vim9script

let start = reltime()

def VimNew(): number
  let sum = 0
  for i in range(1, 2999999)
    sum = sum + 1
  endfor

  return sum
enddef

echo VimNew()
echo reltimestr(reltime(start))
