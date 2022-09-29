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

def Test_inplace_inplace()
  var x = [1, 4, 2, 5]
  x->sort()->filter((_, y) => y % 2 == 0)
  assert_equal([2, 4], x)

  var foo = [1, 4, 2, 5]
  foo = foo->sort()->filter((_, y) => y % 2 == 0)

  assert_equal([2, 4], foo)
enddef

#->filter((_, x) => x % 2 == 0)
#->map((_, y) => y + 1)
#->sort()

# var expr_prec = -1.234->string()
# 
# var foo = base->name(args)
# var foo = base->some.name(args)
# var foo = base->alist[idx](args)
# var foo = base->(getFuncRef())(args)
