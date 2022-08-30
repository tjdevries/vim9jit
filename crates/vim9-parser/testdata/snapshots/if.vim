vim9script

# multiple expressions
if pumvisible() || (str2nr(&t_Co) < 8 && !has('gui_running'))
  finish
endif
