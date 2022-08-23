vim9script

var five = 5
var ten = 10

def Add(x: number, y: number): number
	return x + y
enddef

var result = Add(five, ten)
echo result


var var = 1
var += 1

var number = false
echo number

var if = true
if if
	echo if
endif

var x = 10
if x == 10 || x >= 10 || x <= 10 || x > 10 || x < 10
  x += 1
  x *= 1
  x /= 1
  x -= 1
  echo x
endif
