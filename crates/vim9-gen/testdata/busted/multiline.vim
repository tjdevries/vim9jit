vim9script


def Test_same_line()
  var x: bool = true &&
    false

  assert_equal(x, false)
enddef

def Test_next_line()
  var y: bool = true
    && false

  assert_equal(y, false)
enddef

def Test_missing_lines()
  var z = true

    || false

  assert_equal(z, true)
enddef
