vim9script

var l = [1, 2, 3]

def Test_can_index()
  assert_equal(1, l[0])
enddef

def Test_minus()
  var index = 1
  assert_equal(1, l[index - 1])
enddef

def Test_both()
  assert_equal([1, 2], l[0 : 1])
enddef

def Test_left()
  assert_equal([2, 3], l[1 : ])
enddef

def Test_right()
  assert_equal([1, 2], l[: 1])
enddef

def Test_string()
  var foo = "abcd"
  assert_equal(foo[: -2], "abc")
enddef
