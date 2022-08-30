vim9script

augroup matchparen
  autocmd! CursorMoved,CursorMovedI,WinEnter * {
      echo "Block"
    }

  autocmd WinLeave * echo "Command"
augroup END

var x = len(nvim_get_autocmds({group: "matchparen"}))
