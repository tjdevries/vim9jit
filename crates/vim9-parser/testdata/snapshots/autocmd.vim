vim9script

augroup matchparen
  # Replace all matchparen autocommands
  autocmd! CursorMoved,CursorMovedI,WinEnter * {
      echo "Block"
    }

  autocmd WinLeave * echo "Command"

  autocmd WinLeave *.py echo "snek lang"
  autocmd WinLeave *.py,*.rs echo "One of these is better"

augroup END
