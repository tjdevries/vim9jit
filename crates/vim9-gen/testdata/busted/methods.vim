vim9script

def Test_string_methods()
  var s = "hello"
  assert_equal(s->len(), 5)

  assert_equal("hello"->len(), 5)
enddef


def Test_list_methods()
  var x = [5, 4, 2, 1, 7, 12, 8]
    ->sort()

  assert_equal([1, 12, 2, 4, 5, 7, 8], x)

  var numeric = [5, 4, 2, 1, 7, 12, 8]
    ->sort('n')

  assert_equal([1, 2, 4, 5, 7, 8, 12], numeric)
enddef

def Test_inplace()
  var x = [1, 4, 2, 5]
  x->sort()

  assert_equal([1, 2, 4, 5], x)
enddef

def Test_inplace_inplace_multi()
  var x = [1, 4, 2, 5]
  x->sort()
  x->filter((_, y) => y % 2 == 0)
  assert_equal([2, 4], x)
enddef


def Test_inplace_inplace_single()
  var foo = [1, 4, 2, 5]
  foo->sort()->filter((_, y) => y % 2 == 0)

  assert_equal([2, 4], foo)
enddef
