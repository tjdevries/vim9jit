def Test_method_newline()
  var x = [1, 2, 3]
  x
    ->filter((_, y): bool => y == 2)

  assert_equal([2], x)
enddef
