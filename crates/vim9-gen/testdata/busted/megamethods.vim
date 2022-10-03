vim9script

def Test_inplace_discarded()
  assert_equal([2, 4], [1, 4, 2, 5]->sort()->filter((_, y) => y % 2 == 0))

  var foo = [1, 4, 2, 5]->sort()->filter((_, y) => y % 2 == 0)
  assert_equal([2, 4], foo)
enddef

def Test_returned_foo()
  var foo = [1, 4, 2, 5]
  assert_equal([5, 4, 2, 1], foo->sort()->reverse())
enddef

def Test_returned_foo()
  var foo = [1, 4, 2, 5]
  var bar = foo
  foo->sort()->reverse()
  assert_equal([5, 4, 2, 1], foo)
  assert_equal([5, 4, 2, 1], bar)
  assert_equal(foo, bar)
enddef
