pub const TERRIBLE_BENCHMARK: &str = r#"
var start = reltime()

def VimNew(): number
  var sum = 1

  for i in range(1, 2999999)
    sum = sum + i
  endfor

  return sum
enddef

var sum = VimNew()
"#;
