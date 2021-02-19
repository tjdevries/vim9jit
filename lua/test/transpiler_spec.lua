local transpiler = require('vim9jit.transpiler')

describe('transpiler', function()
  it('should be able to transpile vim9script', function()
    local result = transpiler.transpile [[
      vim9script

      g:MY_VAR = 1
    ]]

    vim.api.nvim_exec(result, false)
    assert.are.same(vim.g.MY_VAR, 1)
  end)

  it('should be able to transpile vimscript with vim9 in it', function()
    local result = transpiler.transpile [[
      function! HelloWorld()
        return "Hello World"
      endfunction

      def FinalFunc(): string
        return HelloWorld()
      enddef

      let g:MY_VAR = FinalFunc()
    ]]

    assert.are.same(result, '')
    vim.api.nvim_exec(result, false)
  end)
end)
