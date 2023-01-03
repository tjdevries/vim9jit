vim9script

def Test_assignment_bool_1()
  var bool1: bool = true
  assert_equal(v:true, bool1)
  var bool2: bool = false
  assert_equal(v:false, bool2)
enddef

def Test_assignment_bool_2()
  var bool3: bool = 0
  assert_equal(false, bool3)
  var bool4: bool = 1
  assert_equal(true, bool4)
enddef

def Test_assignment_bool_3()
  var bool5: bool = 1 && true
  assert_equal(true, bool5)
  var bool6: bool = 0 && 1
  assert_equal(false, bool6)
  var bool7: bool = 0 || 1 && true
  assert_equal(true, bool7)

  # var lines =<< trim END
  #   vim9script
  #   def GetFlag(): bool
  #     var flag: bool = 1
  #     return flag
  #   enddef
  #   var flag: bool = GetFlag()
  #   assert_equal(true, flag)
  #   flag = 0
  #   assert_equal(false, flag)
  #   flag = 1
  #   assert_equal(true, flag)
  #   flag = 1 || true
  #   assert_equal(true, flag)
  #   flag = 1 && false
  #   assert_equal(false, flag)

  #   var cp: bool = &cp
  #   var fen: bool = &l:fen
  # END
  # v9.CheckScriptSuccess(lines)
  # v9.CheckDefAndScriptFailure(['var x: bool = 2'], 'E1012:')
  # v9.CheckDefAndScriptFailure(['var x: bool = -1'], 'E1012:')
  # v9.CheckDefAndScriptFailure(['var x: bool = [1]'], 'E1012:')
  # v9.CheckDefAndScriptFailure(['var x: bool = {}'], 'E1012:')
  # v9.CheckDefAndScriptFailure(['var x: bool = "x"'], 'E1012:')

  # v9.CheckDefAndScriptFailure(['var x: bool = "x"', '', 'eval 0'], 'E1012:', 1)
enddef

def Test_unpacked_identifiers()
  var [x, y] = [1, 2]
  assert_equal(1, x)
  assert_equal(2, y)
enddef

def Test_modifier_prefixes()
  var foo = 10
  --foo
  assert_equal(9, foo)

  ++foo
  ++foo
  assert_equal(11, foo)
enddef


def Test_using_reserved_words()
  var end = true
  assert_equal(end, true)

  var M = true
  assert_equal(M, true)
enddef
