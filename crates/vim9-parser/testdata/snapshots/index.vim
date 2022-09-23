vim9script

def Test_can_index()
  var l = [1, 2, 3]
  assert_equal([1, 2, 3], l)
  assert_equal(1, l[0])
  assert_equal([1, 2], l[0 : 1])
enddef
