# vim9jit

Ok, hear me out. vim9script... but in lua... so fast

## What??

So, here's my plan that I'm messing around with:

1. Write a `vim9script` parser using Lua(jit) & LPEG
2. Write a generator from `vim9script` AST-like nodes -> Lua code.
3. Write wrappers to make it so that when a file sees `vim9script`, what it does instead is generate and expose the code in the correct way.
4. ???
5. Profit.

## Why?

Well if we can just make languages whenever we want, why not make parsers and generators whenever we want :)

Also, fun.

## Performance?

Well, it would be pretty funny if luajit outperformed native vim9script. We'll have to see.

*UPDATE*:

```vim
vim9script

let start = reltime()

def VimNew(): number
  let sum = 0
  for i in range(1, 2999999)
    sum = sum + i
  endfor

  return sum
enddef

echo VimNew()
echo reltimestr(reltime(start))

" Result:
" 4499998500000
" 0.082964
```

```lua
--[=[
Original vimscript
vim9script

let start = reltime()

def VimNew(): number
  let sum = 0
  for i in range(1, 2999999)
    sum = sum + i
  endfor

  return sum
enddef

echo VimNew()
echo reltimestr(reltime(start))

--]=]

local start = vim.fn['reltime']()
local function VimNew()
  local sum = 0
  for i = 1, 2999999, 1 do
    sum = sum + i
  end
  return sum
end
vim.cmd(string.format([[%s %s]], 'echo', VimNew()))
vim.cmd(string.format([[%s %s]], 'echo', vim.fn['reltimestr'](vim.fn['reltime'](start))))

--4499998500000
-- 0.002857
```


So... looks like we're winning ;)

## Non-Goals

I don't think I want to write all the code that actually does the type checking and what not that is now going to be included in vim9script. I think I'll just pass that off to testing your code in vim9 (at least for now).

I will at least attempt to parse and keep that information for use later if desired.

## Examples

You can see the results in `./vim9_scripts/` where there are `*.vim` files and corresponding `*.lua` files that I've generated. I'll be adding more examples there later.
