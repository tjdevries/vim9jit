vim9script noclear
# Vim plugin for showing matching parens
# Maintainer:  Bram Moolenaar <Bram@vim.org>
# Last Change: 2022 Aug 22

# Exit quickly when:
# - this plugin was already loaded (or disabled)
# - 'compatible' is set
if exists('g:loaded_matchparen') || &cp
  finish
endif
g:loaded_matchparen = 1

if !exists('g:matchparen_timeout')
  g:matchparen_timeout = 300
endif
if !exists('g:matchparen_insert_timeout')
  g:matchparen_insert_timeout = 60
endif

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

# Skip the rest if it was already done.
if exists('*Highlight_Matching_Pair')
  finish
endif

# The function that is invoked (very often) to define a ":match" highlighting
# for any matching paren.
def Highlight_Matching_Pair(): void
  # Remove any previous match.
  Remove_Matches()

  # Avoid that we remove the popup menu.
  # Return when there are no colors (looks like the cursor jumps).
  if pumvisible() || (str2nr(&t_Co) < 8 && !has('gui_running'))
    return
  endif

  # Get the character under the cursor and check if it's in 'matchpairs'.
  var c_lnum = line('.')
  var c_col = col('.')
  var before = 0

  var text = getline(c_lnum)
  var matches = matchlist(text, '\(.\)\=\%' .. c_col .. 'c\(.\=\)')
  var [c_before, c] = ['', '']
  if !empty(matches)
    [c_before, c] = matches[1 : 2]
  endif
  var plist = split(&matchpairs, '.\zs[:,]')
  var i = index(plist, c)
  if i < 0
    # not found, in Insert mode try character before the cursor
    if c_col > 1 && (mode() == 'i' || mode() == 'R')
      before = strlen(c_before)
      c = c_before
      i = index(plist, c)
    endif
    if i < 0
      # not found, nothing to do
      return
    endif
  endif

  var s_flags: string
  var c2: string
  # Figure out the arguments for searchpairpos().
  if i % 2 == 0
    s_flags = 'nW'
    c2 = plist[i + 1]
  else
    s_flags = 'nbW'
    c2 = c
    c = plist[i - 1]
  endif
  if c == '['
    c = '\['
    c2 = '\]'
  endif

  # Find the match.  When it was just before the cursor move it there for a
  # moment.
  var save_cursor: list<number>
  if before > 0
    save_cursor = getcurpos()
    cursor(c_lnum, c_col - before)
  endif

  var s_skip: string
  if !has('syntax') || !exists('g:syntax_on')
    s_skip = '0'
  else
    # Build an expression that detects whether the current cursor position is
    # in certain syntax types (string, comment, etc.), for use as
    # searchpairpos()'s skip argument.
    # We match "escape" for special items, such as lispEscapeSpecial, and
    # match "symbol" for lispBarSymbol.
    s_skip = '!empty(filter(map(synstack(line("."), col(".")), ''synIDattr(v:val, "name")''), ' ..
      '''v:val =~? "string\\|character\\|singlequote\\|escape\\|symbol\\|comment"''))'
    # If executing the expression determines that the cursor is currently in
    # one of the syntax types, then we want searchpairpos() to find the pair
    # within those syntax types (i.e., not skip).  Otherwise, the cursor is
    # outside of the syntax types and s_skip should keep its value so we skip
    # any matching pair inside the syntax types.
    # Catch if this throws E363: pattern uses more memory than 'maxmempattern'.
    try
      if eval(s_skip)
        s_skip = "0"
      endif
    catch /^Vim\%((\a\+)\)\=:E363/
      # We won't find anything, so skip searching, should keep Vim responsive.
      return
    endtry
  endif

  # Limit the search to lines visible in the window.
  var stoplinebottom = line('w$')
  var stoplinetop = line('w0')
  var stopline: number
  if i % 2 == 0
    stopline = stoplinebottom
  else
    stopline = stoplinetop
  endif

  var timeout: number
  var m_lnum: number
  var m_col: number
  var adjustedScrolloff: number
  var bottom_viewable: number
  var top_viewable: number
  # Limit the search time to 300 msec to avoid a hang on very long lines.
  # This fails when a timeout is not supported.
  if mode() == 'i' || mode() == 'R'
    timeout = exists('b:matchparen_insert_timeout') ? b:matchparen_insert_timeout : g:matchparen_insert_timeout
  else
    timeout = exists('b:matchparen_timeout') ? b:matchparen_timeout : g:matchparen_timeout
  endif
  try
    [m_lnum, m_col] = searchpairpos(c, '', c2, s_flags, s_skip, stopline, timeout)
  catch /E118/
    # Can't use the timeout, restrict the stopline a bit more to avoid taking
    # a long time on closed folds and long lines.
    # The "viewable" variables give a range in which we can scroll while
    # keeping the cursor at the same position.
    # adjustedScrolloff accounts for very large numbers of scrolloff.
    adjustedScrolloff = min([&scrolloff, (line('w$') - line('w0')) / 2])
    bottom_viewable = min([line('$'), c_lnum + &lines - adjustedScrolloff - 2])
    top_viewable = max([1, c_lnum - &lines + adjustedScrolloff + 2])
    # one of these stoplines will be adjusted below, but the current values are
    # minimal boundaries within the current window
    if i % 2 == 0
      var stopbyte = min([line2byte('$'), line2byte('.') + col('.') + &smc * 2])
      stopline = min([bottom_viewable, byte2line(stopbyte)])
      stoplinebottom = stopline
    else
      var stopbyte = max([1, line2byte('.') + col('.') - &smc * 2])
      stopline = max([top_viewable, byte2line(stopbyte)])
      stoplinetop = stopline
    endif
    [m_lnum, m_col] = searchpairpos(c, '', c2, s_flags, s_skip, stopline)
  endtry

  if before > 0
    setpos('.', save_cursor)
  endif

  # If a match is found setup match highlighting.
  if m_lnum > 0 && m_lnum >= stoplinetop && m_lnum <= stoplinebottom 
    matchaddpos('MatchParen', [[c_lnum, c_col - before], [m_lnum, m_col]], 10, 3)
    w:paren_hl_on = 1
  endif
enddef

def Remove_Matches()
  if exists('w:paren_hl_on') && w:paren_hl_on
    silent! matchdelete(3)
    w:paren_hl_on = 0
  endif
enddef


# Define commands that will disable and enable the plugin.
command DoMatchParen {
    DoMatchParen()
  }
command NoMatchParen {
    NoMatchParen()
  }

def NoMatchParen()
  var w = winnr()
  noau windo silent! call matchdelete(3)
  unlet! g:loaded_matchparen
  exe 'noau :' .. w .. 'wincmd w'
  au! matchparen
enddef

def DoMatchParen()
  runtime plugin/matchparen.vim
  var w = winnr()
  silent windo doau CursorMoved
  exe 'noau :' .. w .. 'wincmd w'
enddef

# vim:ts=2:et:
