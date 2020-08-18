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

## Status

- Can parse
    - [ ] vim9script vs. old
        - [ ] Need to be able to parse an old vim file, and then just the vim9 `def` as well?...
    - [ ] Expressions
        - [x] With type definitions
        - [x] Simple arithmetic expressions
        - [x] String concatentation
        - [x] Multiplication
        - [x] List definitions
        - [ ] Numbers
            - [x] Decimal numbers
            - [x] Floats
            - [x] Hex
            - [ ] Literal blobs
        - [ ] Dictionary definitions
            - [ ] Old style dicts
            - [ ] Literal dicts
        - [ ] Vim-isms
            - [x] Global variables
            - [ ] Buffer variables
            - [ ] Tab variables
            - [ ] Window variables
            - [ ] Script-local variables
                - [ ] Referencing them later
        - [x] Conditionals
        - [ ] If statements
            - [x] Simple if statements
            - [ ] Else statements
            - [ ] Elseif statements
            - [ ] Exact vim semantics for if statements
        - [ ] Function calls
            - [x] Local functions (implemented in pure lua)
            - [x] Builtin functions (called using `vim.fn`)
            - [ ] Script local functions (exposed)
        - [x] Commands
    - [ ] Comments
        - [x] Simple comments
        - [ ] Need to also handle translation of old vim stuff w/ the # command?
    - [ ] Func defitions
        - [x] Function argument typing
        - [x] Function return typing
        - [ ] Optional arguments
        - [ ] Exporting
        - [ ] Spread `def MyFunc(...itemlist: list<number>)`
        - [ ] Legacy function definitions
            - [ ] Legacy function definitions intersperesed.
    - [ ] For loops
        - [x] Simple loops
        - [x] Optimized range
    - [ ] Importing functions
    - [ ] Auto sourcing / finding functions



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


So... looks like we're pretty fast ;)

## Non-Goals

I don't think I want to write all the code that actually does the type checking and what not that is now going to be included in vim9script. I think I'll just pass that off to testing your code in vim9 (at least for now).

I will at least attempt to parse and keep that information for use later if desired.

## Examples

You can see the results in `./vim9_scripts/` where there are `*.vim` files and corresponding `*.lua` files that I've generated. I'll be adding more examples there later.
