vim9script

def Test_simple_heredoc()
  var x =<< END
hello
world
  END

  assert_equal(["hello", "world"], x)
enddef

def Test_simple_heredoc_with_whitespace()
  var x =<< END
    hello
  world
  END

  assert_equal(["    hello", "  world"], x)
enddef

# def Test_simple_heredoc_with_quotes()
#   var x =<< END
#     "hello"
#   world
#   END
# 
#   assert_equal(["    "hello"", "  world"], x)
# enddef
