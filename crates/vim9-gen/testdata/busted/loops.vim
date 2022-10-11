vim9script

def Test_can_do_for_loop()
  var x = 0
  for y in [1, 2, 3]
    x += y
  endfor

  assert_equal(6, x)
enddef

def Test_can_do_for_loop_with_break()
  var x = 0
  for y in [1, 2, 3]
    x += y

    if y == 2
      break
    endif
  endfor

  assert_equal(3, x)
enddef

def Test_can_do_for_loop_with_break_with_continue()
  var x = 0
  for y in [1, 2, 3, 4, 5]
    x += y

    if y == 2
      continue
    endif

    if y == 3
      break
    endif
  endfor

  assert_equal(6, x)
enddef

def Test_can_do_for_loop_with_continue()
  var x = 0
  for y in [1, 2, 3]
    if y == 2
      continue
    endif

    x += y
  endfor

  assert_equal(4, x)
enddef

def Test_can_do_for_loop_with_return()
  def Something(): number
    var x = 0
    for y in [1, 2, 3]
      if y == 2
        return 5
      endif

      if y == 10
        continue
      endif

      x += y
    endfor

    return 0
  enddef

  assert_equal(5, Something())
enddef
