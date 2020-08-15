--[=[
Original vimscript
vim9script

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

  # let RetOne: func(string): number = function('len')
  # let RetTwo: func(string): number = function('winnr')
  # let RetThat: func = g:atrue ? RetOne : RetTwo
  # assert_equal(function('len'), RetThat)

  # let X = FuncOne
  # let Y = FuncTwo
  # let Z = g:cond ? FuncOne : FuncTwo
  # assert_equal(123, Z(3))

enddef
--]=]


local function Test_expr1()
  vim.fn['assert_equal']('one', vim9jit.conditional(true, 'one', 'two'))
  vim.fn['assert_equal']('one', vim9jit.conditional(1, 'one', 'two'))
  vim.fn['assert_equal']('one', vim9jit.conditional('x', 'one', 'two'))
  vim.fn['assert_equal']('one', vim9jit.conditional('x', 'one', 'two'))

  local var = 1
  vim.fn['assert_equal']('one', vim9jit.conditional(var, 'one', 'two'))

  vim.fn['assert_equal']('two', vim9jit.conditional(false, 'one', 'two'))
  vim.fn['assert_equal']('two', vim9jit.conditional(0, 'one', 'two'))

  vim.fn['assert_equal']('two', vim9jit.conditional('', 'one', 'two'))
  var = 0
  vim.fn['assert_equal']('two', vim9jit.conditional(var, 'one', 'two'))

  vim.fn['assert_equal']('one', vim9jit.conditional({ 0 }, 'one', 'two'))
  vim.fn['assert_equal']('one', vim9jit.conditional({ 0, 1, 2 }, 'one', 'two'))
  vim.fn['assert_equal']('two', vim9jit.conditional({  }, 'one', 'two'))

  if vim.fn['has']('float') then
    vim.fn['assert_equal']('one', vim9jit.conditional(0.1, 'one', 'two'))
  end
  if vim.fn['has']('float') then
    vim.fn['assert_equal']('two', vim9jit.conditional(0.0, 'one', 'two'))
  end
  -- # Not supported yet
  --   #
  --   # assert_equal('one', 0z1234 ? 'one' : 'two')
  --   # assert_equal('one', #{x: 0} ? 'one' : 'two')
  --   # assert_equal('two', 0z ? 'one' : 'two')
  --   # assert_equal('two', {} ? 'one' : 'two')

  local Some = vim9jit.vim_function('len')
  local Other = vim9jit.vim_function('winnr')
  vim.g["Res"] = vim9jit.conditional(vim.g['atrue'], Some, Other)
  vim.fn['assert_equal'](vim9jit.vim_function('len'), Res)

  --   # let RetOne: func(string): number = function('len')
  --   # let RetTwo: func(string): number = function('winnr')
  --   # let RetThat: func = g:atrue ? RetOne : RetTwo
  --   # assert_equal(function('len'), RetThat)

  --   # let X = FuncOne
  --   # let Y = FuncTwo
  --   # let Z = g:cond ? FuncOne : FuncTwo
  --   # assert_equal(123, Z(3))
end
