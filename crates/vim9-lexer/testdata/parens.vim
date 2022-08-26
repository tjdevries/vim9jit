vim9script

var x = 5
x += 1
echo x
echo (x)
echo	(x)

var xyz = +1
echo xyz
echo (xyz)

def g:MyFunc(): bool
	var return = false
	var var = return
	return return
enddef

g:MyThing = 1
g:["MyOtherThing"] = 2
echo g:MyOtherThing

g:MyFunc()
