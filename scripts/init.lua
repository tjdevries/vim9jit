vim.opt.rtp:append({
  '.',
  vim.fn.expand('~/.local/share/nvim/site/pack/vendor/start/plenary.nvim'),
})

vim.cmd([[runtime! plugin/plenary.vim]])
