vim9script

var foo = [1]
foo->sort()->filter((_, y) => y % 2 == 0)
