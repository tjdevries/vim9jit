--[=[
Original vimscript
vim9script

let g:cond = v:false

def FuncOne(arg: number): string
  return 'yes'
enddef

def FuncTwo(arg: number): number
  return 123
enddef

def Test_expr1()
  assert_equal('one', true ? 'one' : 'two')
  assert_equal('one', 1 ?
			'one' :
			'two')
  assert_equal('one', 'x' ? 'one' : 'two')
  assert_equal('one', 'x'
  			? 'one'
			: 'two')

  let var = 1
  assert_equal('one', var ? 'one' : 'two')

  assert_equal('two', false ? 'one' : 'two')
  assert_equal('two', 0 ? 'one' : 'two')

  assert_equal('two', '' ? 'one' : 'two')
  var = 0
  assert_equal('two', var ? 'one' : 'two')

  assert_equal('one', [0] ? 'one' : 'two')
  assert_equal('one', [0, 1, 2] ? 'one' : 'two')
  assert_equal('two', [] ? 'one' : 'two')

  if has('float')
    assert_equal('one', 0.1 ? 'one' : 'two')
  endif

  if has('float')
    assert_equal('two', 0.0 ? 'one' : 'two')
  endif

  # Not supported yet
  #
  # assert_equal('one', 0z1234 ? 'one' : 'two')
  # assert_equal('one', #{x: 0} ? 'one' : 'two')
  # assert_equal('two', 0z ? 'one' : 'two')
  # assert_equal('two', {} ? 'one' : 'two')

  let Some: func = function('len')
  let Other: func = function('winnr')
  let Res: func = g:atrue ? Some : Other
  assert_equal(function('len'), Res)

  let RetOne: func(string): number = function('len')
  let RetTwo: func(string): number = function('winnr')
  let RetThat: func = g:atrue ? RetOne : RetTwo
  assert_equal(function('len'), RetThat)

  let X = FuncOne
  let Y = FuncTwo
  let Z = g:cond ? FuncOne : FuncTwo
  assert_equal(123, Z(3))
enddef
--]=]

require('vim9jit')

