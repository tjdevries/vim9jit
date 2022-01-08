pub const TERRIBLE_BENCHMARK: &str = r#"
var start = reltime()

def VimNew(): number
  var sum = 0
  for i in range(1, 2999999)
  endfor

  return sum
enddef
"#;
