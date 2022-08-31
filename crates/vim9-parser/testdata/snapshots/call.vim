vim9script

# No arguments
var no_args = bufnr()

# One argument
var some_args = len("hello world")

# More arguments
var matched = match(["matched", "some_args"], "a", 1)
