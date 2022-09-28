vim9script

# multiple expressions
if pumvisible() || (str2nr(&t_Co) < 8 && !has('gui_running'))
  finish
endif

var x = 20
if x < 10
  echo "10"
elseif x < 15
  echo "15"
elseif x < 30
  echo "30"
else
  echo "else"
endif
