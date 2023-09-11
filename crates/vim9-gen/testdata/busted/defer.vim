
# def AddDefer(arg1)
#   call extend(g:deferred, [arg1])
#   # if a:0 == 1
#   #   call extend(g:deferred, [1])
#   # endif
# enddef
# 
# def WithDeferTwo()
#   call extend(g:deferred, ['in Two'])
#   for nr in range(3)
#     defer AddDefer('Two' .. nr)
#   endfor
#   call extend(g:deferred, ['end Two'])
# enddef
# 
# def WithDeferOne()
#   call extend(g:deferred, ['in One'])
#   defer AddDefer('One')
#   call WithDeferTwo()
#   call extend(g:deferred, ['end One'])
# 
#   # call writefile(['text'], 'Xfuncdefer')
#   # defer delete('Xfuncdefer')
# enddef
# 
# def Test_defer()
#   g:deferred = []
#   call WithDeferOne()
# 
#   call assert_equal(['in One', 'in Two', 'end Two', 'Two2', 'Two1', 'Two0', 'end One', 'One'], g:deferred)
# enddef

def MyDefer()
  var x = []
  defer add(x, 1)
  defer add(x, 2)
  defer add(x, 3)

  return x
enddef

def RangeDefer()
  var x = []

  for i in range(3)
    defer add(x, i)
  endfor

  return x
enddef

def Test_defer()
  var x = MyDefer()
  assert_equal([3, 2, 1], x)

  var y = RangeDefer()
  assert_equal([2, 1, 0], y)
enddef
