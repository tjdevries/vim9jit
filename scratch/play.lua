local dkjson = require('dkjson')
local helpers = require('test.helpers')
local parser = require('vim9jit.parser')
local token = require('vim9jit.token')



-- parser ops {{{

local function parse_full_file(s)
  return token.parsestring(parser.grammar, s)
end

local function parse_fake_file(s)
  return token.parsestring(parser.grammar, 'vim9script\n' .. helpers.dedent(s))
end

local function parse_expression(s)
  return token.parsestring(parser.grammar_expression, s)
end

-- }}}


-- file tools {{{

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


-- simple fmt {{{

local simple_order = {
  id=10,
  value=20,
  pos=30,
  line_start=31,
  line_finish=32,
  char_start=33,
  char_finish=34,
  byte_start=35,
  byte_finish=36,
}

local function keys_list(t, keys_order)
  local ks = {}
  for k in pairs(t) do
    ks[#ks+1] = k
  end
  table.sort(ks, function(a, b)
    local p = keys_order[a]
    local q = keys_order[b]
    if p and q then
      return p < q
    elseif p then
      return true
    elseif q then
      return false
    else
      return a < b
    end
  end)
  return ks
end

local function simple_format(node, order, depth)
  if type(node) == "string" then
    return string.format("%q", node):gsub("\\\n", "\\n")
  end
  if type(node) == "number" then
    return node
  end

  assert(type(node) == "table")

  order = order or simple_order
  depth = depth or 0

  local dump = {}
  local cent = "  "
  local dent = string.rep(cent, depth)

  dump[#dump+1] = "{\n"
  local keys = keys_list(node, order)
  for i=1,#keys do
    local k = keys[i]
    local ksafe = (type(k) == "string" and k) or ("[" .. k .. "]")
    dump[#dump+1] = cent .. ksafe .. "=" .. simple_format(node[k], order, depth+1) .. ",\n"
  end
  dump[#dump+1] = "}"

  if depth == 0 then
    dump[#dump+1] = "\n"
  end

  return table.concat(dump, dent)
end

-- }}}


-- really run {{{

if not pcall(debug.getlocal, 4, 1) then
  local tested = {
    "def_func",
    "def_var",
    "run_perf",
  }


  local ok = true
  local function OK(fn)
    local function tb(...)
      io.stderr:write("\n", debug.traceback(...), "\n\n")
      io.stderr:flush()
    end
    ok = xpcall(fn, tb) and ok
  end

  for _, n in ipairs(tested) do
    local p, q

    OK(function()
      p = FR("./test.vim9/" .. n .. ".vim", parse_full_file)
    end)

    OK(function()
      q = FW("./test.lpeg/" .. n .. ".lpeg", simple_format(p))
    end)
  end

  if not ok then
    os.exit(2)
  end
end
-- }}}
