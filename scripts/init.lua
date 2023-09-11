-- Add target dir, if not exists
vim.fn.mkdir('target', 'p')

if not vim.loop.fs_stat('target/plenary.nvim') then
  vim
    .system({ 'git', 'clone', 'https://github.com/nvim-lua/plenary.nvim', 'target/plenary.nvim' })
    :wait()
end

vim.opt.rtp:append({ '.', './target/plenary.nvim' })

vim.cmd([[runtime! plugin/plenary.vim]])
