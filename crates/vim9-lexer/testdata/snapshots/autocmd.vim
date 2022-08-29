vim9script

augroup matchparen
  # Replace all matchparen autocommands
  autocmd! CursorMoved,CursorMovedI,WinEnter * {
      Highlight_Matching_Pair()
    }
  autocmd! WinLeave * {
      Remove_Matches()
    }
  autocmd! TextChanged,TextChangedI * {
      Highlight_Matching_Pair()
    }
augroup END
