vim9script

# Able to parse multiple commands in one
command! BarredCommand echo "x" | echo "y"

# Able to parse multiple function calls
command! FuncCommand x.Something().Another() | AnotherThing()

# Able to parse multiple calls in one command
command! MinitermToggle     miniterm.GetManager().ToggleTerminal()

command! MinitermToggle     miniterm.GetManager().ToggleTerminal() | g:AttachWipeoutHandler(miniterm.GetManager().current)
