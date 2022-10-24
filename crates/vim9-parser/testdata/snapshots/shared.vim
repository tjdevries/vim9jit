vim9script

syn keyword contextConstants zerocount minusone minustwo plusone plustwo contained
syn keyword contextConstants plusthree plusfour plusfive plussix plusseven contained

def Test_syn()
  syn keyword Test testkeyword contained
  assert_equal(2, execute('syntax list Test')->split("\n")->len())
enddef

