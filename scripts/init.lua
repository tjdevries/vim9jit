local plenary_path = vim.fn.expand('~/.local/share/nvim/site/pack/vendor/start/plenary.nvim') --[[@as string]]

vim.opt.rtp:append({
  '.',
  plenary_path,
})

vim.cmd([[runtime! plugin/plenary.vim]])
