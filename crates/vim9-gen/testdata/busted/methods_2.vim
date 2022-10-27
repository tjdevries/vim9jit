def Test_method_comments()
  var x = [1, 2, 3]
  x
    # Remove members, these can't appear without something in front.
    ->filter((_, y): bool =>
              y == 2)

  assert_equal(x, [2])
enddef
