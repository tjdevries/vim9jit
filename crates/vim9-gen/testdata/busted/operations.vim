vim9script

def Test_operations()
  var bool5: bool = 1 && true
  assert_equal(true, bool5)
  var bool6: bool = 0 && 1
  assert_equal(false, bool6)
  var bool7: bool = 0 || 1 && true
  assert_equal(true, bool7)
enddef

def Test_lsp_from_yega()
  var x = true
  if !has('vim9script') ||  v:version < 900
    x = false
  endif

  assert_equal(true, x)
enddef
