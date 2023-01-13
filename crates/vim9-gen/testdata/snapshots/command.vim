vim9script

command! MultipleEcho echo "x" | echo "y"

command! MinitermToggle     miniterm.GetManager().ToggleTerminal() | g:AttachWipeoutHandler(miniterm.GetManager().current)
