vim9script

var vx: number = 101
var vy: number = 102

var vz = 12345

def MyFunction(): number
	var return: number = 1
	echo return

	return = return + 1
	return return
enddef

echo MyFunction()
