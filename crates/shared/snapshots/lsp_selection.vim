vim9script

# Thanks to: https://github.com/yegappan/lsp for some test cases

# Functions related to handling LSP range selection.

import './util.vim'

# Visually (character-wise) select the text in a range
def SelectText(bnr: number, range: dict<dict<number>>)
  var start_col: number = util.GetLineByteFromPos(bnr, range.start) + 1
  var end_col: number = util.GetLineByteFromPos(bnr, range.end)

  :normal! v"_y
enddef
