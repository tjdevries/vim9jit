let start = reltime()

function VimNew()
  let sum = 0
  for i in range(1, 2999999)
    let sum = sum + i
  endfor

  return sum
endfunction

echo VimNew()
echo reltimestr(reltime(start))

