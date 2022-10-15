vim9script

var s = "hello"
var l = s->len()

var l2 = "hello"->len()

var x = [5, 4, 2, 1, 7, 12, 8]->sort()
     ->filter((_, x) => x % 2 == 0)
     ->map((_, y) => y + 1)
     ->sort()

var expr_prec = -1.234->string()

var foo = base->name(args)
var foo = base->some.name(args)
var foo = base->alist[idx](args)
var foo = base->(getFuncRef())(args)

var x = [1, 4, 2, 5]
x->sort()

var foo = [slnum, scol]->cursor()

# Testing
[slnum, scol]->cursor()

