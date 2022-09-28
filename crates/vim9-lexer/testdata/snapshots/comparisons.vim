vim9script

var x = 10
if x == 10 || x >= 10 || x <= 10 || x > 10 || x < 10
  x += 1
  x *= 1
  x /= 1
  x -= 1
  echo x
endif

var a = "hello"
var b = "world"

var equal = a == b || a ==# b
var equal_ins = a ==? b

var neq = a != b || a !=# b
var neq_ins = a !=? b

var gt = a > b || a ># b
var gt_ins = a >? b

var gte = a >= b || a >=# b
var gte_ins = a >=? b

var lt = a < b || a <# b
var lt_ins = a <? b

var lte = a <= b || a <=# b
var lte_ins = a <=? b

var reg = a =~ b || a =~# b
var reg_ins = a =~? b

var no_reg = a !~ b || a !~# b
var no_reg_ins = a !~? b

var is_ = a is b || a is# b
var is__ins = a is? b

var is_not = a isnot b || a isnot# b
var is_not_ins = a isnot? b
