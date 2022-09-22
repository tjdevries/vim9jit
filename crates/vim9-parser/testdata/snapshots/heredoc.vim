vim9script

var x =<< END
  hello
  world
  this
END

var y =<< trim eval END
  hello
  world
  +===???asdf<
  this
  too
END
