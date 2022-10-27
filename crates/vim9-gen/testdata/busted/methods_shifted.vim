def Test_settabvar_method()
  call settabvar(1, "testing", 0)
  assert_equal(0, t:testing)

  25->settabvar(1, "testing")
  assert_equal(25, t:testing)
enddef
