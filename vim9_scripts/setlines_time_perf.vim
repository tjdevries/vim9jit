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
