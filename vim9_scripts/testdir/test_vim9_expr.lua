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


vim.g["cond"] = false

local function FuncOne()
  return 'yes'
end

local function FuncTwo()
  return 123
end

local function Test_expr1()
  vim.fn['assert_equal']('one', vim9jit.conditional(true, function() return 'one' end, function() return 'two' end))
  vim.fn['assert_equal']('one', vim9jit.conditional(1, function() return 'one' end, function() return 'two' end))
  vim.fn['assert_equal']('one', vim9jit.conditional('x', function() return 'one' end, function() return 'two' end))
  vim.fn['assert_equal']('one', vim9jit.conditional('x', function() return 'one' end, function() return 'two' end))

  local var = 1
  vim.fn['assert_equal']('one', vim9jit.conditional(var, function() return 'one' end, function() return 'two' end))

  vim.fn['assert_equal']('two', vim9jit.conditional(false, function() return 'one' end, function() return 'two' end))
  vim.fn['assert_equal']('two', vim9jit.conditional(0, function() return 'one' end, function() return 'two' end))

  vim.fn['assert_equal']('two', vim9jit.conditional('', function() return 'one' end, function() return 'two' end))
  var = 0
  vim.fn['assert_equal']('two', vim9jit.conditional(var, function() return 'one' end, function() return 'two' end))

  vim.fn['assert_equal']('one', vim9jit.conditional({ 0 }, function() return 'one' end, function() return 'two' end))
  vim.fn['assert_equal']('one', vim9jit.conditional({ 0, 1, 2 }, function() return 'one' end, function() return 'two' end))
  vim.fn['assert_equal']('two', vim9jit.conditional({  }, function() return 'one' end, function() return 'two' end))

  if vim.fn['has']('float') then
    vim.fn['assert_equal']('one', vim9jit.conditional(0.1, function() return 'one' end, function() return 'two' end))
  end
  if vim.fn['has']('float') then
    vim.fn['assert_equal']('two', vim9jit.conditional(0.0, function() return 'one' end, function() return 'two' end))
  end
  -- # Not supported yet
  --   #
  --   # assert_equal('one', 0z1234 ? 'one' : 'two')
  --   # assert_equal('one', #{x: 0} ? 'one' : 'two')
  --   # assert_equal('two', 0z ? 'one' : 'two')
  --   # assert_equal('two', {} ? 'one' : 'two')

  local Some = vim9jit.vim_function('len')
  local Other = vim9jit.vim_function('winnr')
  local Res = vim9jit.conditional(vim.g['atrue'], function() return Some end, function() return Other end)
  vim.fn['assert_equal'](vim9jit.vim_function('len'), Res)

  local RetOne = vim9jit.vim_function('len')
  local RetTwo = vim9jit.vim_function('winnr')
  local RetThat = vim9jit.conditional(vim.g['atrue'], function() return RetOne end, function() return RetTwo end)
  vim.fn['assert_equal'](vim9jit.vim_function('len'), RetThat)

  local X = FuncOne
  local Y = FuncTwo
  local Z = vim9jit.conditional(vim.g['cond'], function() return FuncOne end, function() return FuncTwo end)
  vim.fn['assert_equal'](123, Z(3))
end
