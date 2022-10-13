vim9script

var x = { hello: "world", another: false }
var y = len(nvim_get_autcomds({group: "matchparen"}))

var multiline = {
  x: x
}

var multiline_trail = {
  x: x,
}

var multiline_two = {
  x: x,
  y: y
}

var multiline_trail = {
  x: x,
  y: y,




}

var keyed = {
    ["hello"]: "world",
    [$'{bnr}']: diag_by_lnum,
}
