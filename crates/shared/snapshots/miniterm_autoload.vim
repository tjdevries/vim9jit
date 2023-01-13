vim9script

# Thanks to https://github.com/hahdookin/miniterm.vim for the test file

# Configuration
# g:miniterm_proportion = get(g:, "miniterm_proportion", 0.28)
# g:miniterm_position = get(g:, "miniterm_position", "bottom")

# These two functions handle destroying buffers.
# Slight workaround to event handling while also
# being able to reference the "manager" in the
# event handling code.
def g:AttachWipeoutHandler(term: dict<any>)
    execute "autocmd BufWipeout <buffer=" .. term.bufnr .. "> vim9 miniterm#GetManager().RemoveBufnr(" .. term.bufnr .. ") | miniterm#GetManager().UpdateState()"
    execute "autocmd BufDelete <buffer=" .. term.bufnr .. "> vim9 miniterm#GetManager().RemoveBufnr(" .. term.bufnr .. ") | miniterm#GetManager().UpdateState()"
enddef

# Terminal class
def Terminal(): dict<any>
    var Res: dict<any> = {
        bufnr: 0,
        winnr: 0,
        open: 0,
    }

    Res.bufnr = term_start($SHELL, { 
        hidden: 1, 
        term_kill: 'hup' 
    })
    setbufvar(Res.bufnr, "&buflisted", 0)

    return Res
enddef

# Terminal manager class
def TerminalManager(): dict<any>
    var Res: dict<any> = {
        terminals: [],
        current: {}
    }

    # Whether or not the manager has a current terminal
    def HasCurrent(): bool
        return !empty(Res.current)
    enddef
    Res.HasCurrent = HasCurrent

    # Open the current terminal
    def OpenCurrent()
        exec "bot sbuffer " .. Res.current.bufnr
        exec "resize " .. float2nr(&lines * g:miniterm_proportion)
        setlocal winfixheight
        setlocal nonumber norelativenumber
        setlocal hidden
        Res.current.winnr = win_getid()
        Res.current.open = 1
    enddef
    Res.OpenCurrent = OpenCurrent

    # Close the current terminal
    def CloseCurrent()
        win_execute(Res.current.winnr, 'close!')
        Res.current.open = 0
    enddef
    Res.CloseCurrent = CloseCurrent

    # Returns what index the terminal is in the list
    def IndexOfTerm(term: dict<any>): number
        var index = -1
        for i in range(len(Res.terminals))
            var t = Res.terminals[i]
            if t.bufnr == term.bufnr
                index = i
                break
            endif
        endfor
        return index
    enddef
    Res.IndexOfTerm = IndexOfTerm

    # Removes a terminal with bufnr from list if it exists
    def RemoveBufnr(bufnr: number)
        for i in range(len(Res.terminals))
            var t = Res.terminals[i]
            if t.bufnr == bufnr
                Res.terminals->remove(i)
                break
            endif
        endfor
    enddef
    Res.RemoveBufnr = RemoveBufnr

    # Create a new terminal and set it as the current
    def CreateNewCurrent()
        var term = Terminal()
        #term.SetName("MT" .. Res.terminals->len())
        Res.terminals->add(term)
        Res.current = term
    enddef
    Res.CreateNewCurrent = CreateNewCurrent

#   - if first time (no current terminal buffer):
#       - run "Toggle" logic
#   - else
#       - if terminal window is open
#           - close that terminal window
#       - create new terminal buffer
#       - open new terminal window
    def NewTerminal()
        if !Res.HasCurrent()
            Res.ToggleTerminal()
        else
            if Res.current.open
                Res.CloseCurrent()
            endif
            Res.CreateNewCurrent()

            Res.OpenCurrent()
        endif
    enddef
    Res.NewTerminal = NewTerminal

#   - if first time (no current terminal buffer):
#       - create a new terminal buffer
#       - set new buffer as current
#   - if terminal window is open:
#       - close terminal window
#   - else if terminal window isnt open:
#       - open terminal window
    def ToggleTerminal()
        if !Res.HasCurrent()
            Res.CreateNewCurrent()
        endif
        if Res.current.open
            Res.CloseCurrent()
        else
            Res.OpenCurrent()
        endif
    enddef
    Res.ToggleTerminal = ToggleTerminal

    # Get index of current terminal in terminal list
    def CurrentIndex(): number
        return Res.IndexOfTerm(Res.current)
    enddef
    Res.CurrentIndex = CurrentIndex

    # Swap current terminal to another based on an index offset
    def OffsetTerminal(offset: number)
        if len(Res.terminals) > 0
            var next_index = (Res.CurrentIndex() + offset) % len(Res.terminals)
            while next_index < 0
                next_index += len(Res.terminals)
            endwhile
            Res.CloseCurrent()
            Res.current = Res.terminals[next_index]
            Res.OpenCurrent()
        endif
    enddef
    Res.OffsetTerminal = OffsetTerminal

    # Delete the current terminal window and do NOT reopen
    def DeleteCurrent()
        if Res.HasCurrent()
            # Wipe current's buffer
            execute "bwipeout! " .. Res.current.bufnr
        endif
    enddef
    Res.DeleteCurrent = DeleteCurrent

    # Wipe all buffers managed by this
    def DeleteAll()
        var buf_nums = mapnew(Res.terminals, (_, v) => v.bufnr)
        for buf_num in buf_nums
            execute "bwipeout! " .. buf_num
        endfor
        Res.current = {}

        Assert(len(Res.terminals) == 0, "DeleteAll did not clear manager's terminals list")
        Res.terminals = [] # FIXME: Redundant, handled by delete handler
    enddef
    Res.DeleteAll = DeleteAll

    # Prints out all active terminals
    # i.e. 0 1 [2] 3
    # if terminal at index 2 is the current
    def ListTerminals()
        if len(Res.terminals) == 0
            echo "No terminals active"
            return
        endif
        final indices = []
        for i in range(len(Res.terminals))
            const term = Res.terminals[i]
            if term.bufnr == Res.current.bufnr
                indices->add('[' .. i .. ']')
            else
                indices->add(i)
            endif
        endfor
        echo join(indices, ' ')
    enddef
    Res.ListTerminals = ListTerminals

    def UpdateState()
        if empty(Res.terminals)
            Res.current = {}
        else
            Res.current = Res.terminals[0]
            Res.OpenCurrent()
        endif
    enddef
    Res.UpdateState = UpdateState

    return Res
enddef

####################################
# Helpers
def Assert(cond: bool, msg: string)
    if !cond
        throw msg
    endif
enddef

final manager = TerminalManager()
export def g:GetManager(): dict<any>
    return manager
enddef
