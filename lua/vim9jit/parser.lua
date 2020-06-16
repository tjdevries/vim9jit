-- local l = require('lpeg')

package.loaded['vim9jit.patterns'] = nil
package.loaded['vim9jit.token'] = nil

local patterns = require('vim9jit.patterns')
local token = require('vim9jit.token')

local V = patterns.V

local whitespace = patterns.set(
  ' ',
  '\t',
  '\v',
  '\f'
)
local EOL = patterns.branch(
  patterns.end_of_line,
  patterns.end_of_file
)

local digit = patterns.range('0', '9')
local letter = patterns.branch(
  patterns.range('a', 'z'),
  patterns.range('A', 'Z')
)

local identifier = patterns.any_amount(letter)

local grammar = token.define(function(_ENV)
  START "vim9script"

  vim9script = patterns.capture(patterns.concat(
    patterns.literal("vim9script"), EOL,
    patterns.any_amount(V("Let"))
  ))

  Number = patterns.capture(patterns.one_or_more(digit))

  Let = patterns.capture(patterns.concat(
    patterns.literal("let"),
    patterns.any_amount(whitespace),
    identifier,
    patterns.any_amount(whitespace),
    patterns.literal("="),
    patterns.any_amount(whitespace),
    V("Number"),
    EOL
  ))

end)

return {
  grammar = grammar
}
