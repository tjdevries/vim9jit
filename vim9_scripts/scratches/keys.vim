vim9script

var x = { [1234]: true }

x[1234] = false

echo x[1234]
echo x.1234
echo x["1234"]

echo x
