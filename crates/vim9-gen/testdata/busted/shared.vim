vim9script

def Test_syn()
  syn keyword Test testkeyword contained
  assert_equal(2, execute('syntax list Test')
        ->split("\n")
        ->len())
enddef

