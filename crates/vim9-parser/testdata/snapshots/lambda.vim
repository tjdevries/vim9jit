vim9script

var X = (a, b) => a + b
echo X(1, 2)

var Y = (a, b) =>
  a + b
echo Y(3, 4)

var Typed = (a: number, b: number): number => a + b
echo Typed(5, 6)
