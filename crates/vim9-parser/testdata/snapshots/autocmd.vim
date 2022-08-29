vim9script

augroup matchparen
  # Replace all matchparen autocommands
  autocmd! CursorMoved,CursorMovedI,WinEnter * {
      echo "Block"
    }

  autocmd WinLeave * echo "Command"
augroup END
