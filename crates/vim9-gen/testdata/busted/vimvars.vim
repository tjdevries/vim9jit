vim9script

def Test_naked_g()
  g:test_var = 5
  assert_equal(5, get(g:, 'test_var'))
  assert_equal(6, get(g:, 'fake_var', 6))
enddef
