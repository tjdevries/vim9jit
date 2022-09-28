vim9script

def Test_default_args()
  def MyCoolFunc(x = 5)
    return x
  enddef

  assert_equal(MyCoolFunc(), 5)
  assert_equal(MyCoolFunc(10), 10)
enddef
