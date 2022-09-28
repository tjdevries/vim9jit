vim9script

var l = [1, 2, 3]
def Test_expression()
  assert_equal(l[-1 :], [3])
enddef
