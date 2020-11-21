local dkjson = require('dkjson')
local helpers = require('test.helpers')
local parser = require('vim9jit.parser')
local token = require('vim9jit.token')



local function parsed_expr(s)
  return token.parsestring(parser.grammar_expression, s)
end


local function parsed_file(s)
  return token.parsestring(parser.grammar, 'vim9script\n' .. helpers.dedent(s))
end


local function parsed_full(s)
  return token.parsestring(parser.grammar, s)
end


local format = {
  indent = true,
  keyorder = {
    "id",
    "value",
    "pos",
    "line_start",
    "line_finish",
    "char_start",
    "char_finish",
    "byte_start",
    "byte_finish",
  },
}

local tested = {
  "def_func",
  "def_var",
  "run_perf",
}


-- file ops {{{
local function FR(n, t)
  local f = io.open(n, "r")
  assert(f)
  local d = f:read("*all")
  f:close()
  return t(d or "")
end

local function FW(n, d)
  local f = io.open(n, "w")
  assert(f)
  f:write(d)
  f:flush()
  f:close()
  return d
end
-- }}}

-- err logs {{{
local ok = true
local function OK(fn)
  local function tb(...)
    io.stderr:write("\n", debug.traceback(...), "\n\n")
    io.stderr:flush()
  end
  ok = xpcall(fn, tb) and ok
end  
-- }}}

for _, n in ipairs(tested) do
  local p, q

  local function pretty_format(node, stack, line)
    return ""
  end

  OK(function()
    p = FR("./test.vim9/" .. n .. ".vim", parsed_full)
  end)

  OK(function()
    q = FW("./test.json/" .. n .. ".json", dkjson.encode(p, format))
  end)

  OK(function()
    q = FW("./test.lpeg/" .. n .. ".lpeg", pretty_format(p, {}, 1))
  end)
end

if not ok then
  os.exit(2)
end
