vim9script

var five = 5
var ten = 10

def Add(x: number, y: number): number
	return x + y
enddef

var result = Add(five, ten)
echo result

