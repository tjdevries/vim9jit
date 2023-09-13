# vim9jit

vim9jit is an experimental project to take `vim9script` and convert it to `lua` that can run within Neovim.

It will not be a full vimscript parser, but instead will only parse the necessary amount of vim9script to send
the rest of the vimscript to Neovim's vimscript interpreter. In general, this revolves around converting new `def`
style syntax, as well as doing things with `import` and a few more items.

For examples of some of the "transpiled" code, you can check: ./crates/vim9-gen/testdata/output/ 
