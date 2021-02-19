local grammar = require('vim9jit.parser').grammar
local grammar_def = require('vim9jit.parser').grammar_def
local token = require('vim9jit.token')

local dedent = require('vim9jit.utils').dedent
local indent = require('vim9jit.utils').indent
local trim = vim.trim

local has_inspect, inspect = pcall(require, 'inspect')
if not has_inspect then
  inspect = vim.inspect
end

local fmt = function(s, ending_newline)
  if string.sub(s, 1, 1) == "\n" then
    s = string.sub(s, 2)
  end

  local eol_string = ''
  if ending_newline == nil or ending_newline == true then
     eol_string = "\n"
  end


  return trim(dedent(s)) .. eol_string
end

-- TODO: Need to extend this further and make the corresponding vim9jit functions to
--          actually do the type checking. That's for another day though.
--          Currently it seems *possible* to do it given the information we've got when parsing at least.

local generator = {}

generator.strict = false

generator.generate = function(str, strict)
  generator.strict = strict

  local parsed = token.parsestring(grammar, str)
  if parsed == nil then
    error('Unparsed token: ' .. inspect(str))
  end

  local output = "require('vim9jit')\n"
  for _, v in ipairs(parsed) do
    local g = assert(generator.match[v.id], v.id)
    output = output .. g(v)
  end

  return output
end

generator.generate_def = function(str, strict)
  generator.strict = strict

  local parsed = token.parsestring(grammar_def, str)
  if parsed == nil then
    error('Unparsed token: ' .. vim.inspect(str))
  end

  local output = "require('vim9jit')\n"
  local g = assert(require('vim9jit.generator.modes.def')[parsed.id], parsed.id)
  output = output .. g(parsed)

  return output
  -- return require("generator.def").generate(str)
end


generator._utils = {}
generator._utils.fmt = fmt

generator._utils.set_grammar = function(g)
  grammar = g
end

generator._utils.get_grammar = function()
  return grammar
end

return generator
