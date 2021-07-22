-- Default to not printing anything during the tests.
-- We can turn it on or off for specific tests if we want
-- local token = require('vim9jit.token')
-- token.print_error = false

local function trim(s)
  return s:match('^%s*(.*%S)') or ''
end

local eq = function(exp, act)
  return require('luassert').are.same(exp, act)
end

local neq = function(exp, act)
  return require('luassert').are_not.same(exp, act)
end

local get_first_item = function(t)
  return t[1][1][1]
end

local get_item

get_item = function(t, param, key, result_number, current_found, recursive)
  if t == nil then
    return nil
  end

  if result_number == nil then result_number = 1 end
  if current_found == nil then current_found = 0 end
  if recursive == nil then recursive = true end

  if t[param] == key then
    return t
  end

  for k, _ in ipairs(t) do
    if t[k][param] == key then
      current_found = current_found + 1

      if current_found >= result_number then
        return t[k]
      end
    end
  end

  if not recursive then return nil end

  local result = nil
  for _, v in ipairs(t) do
    if type(v) == 'table' then
      result = get_item(v, param, key, result_number, current_found)
      if (result) then
        current_found = current_found + 1
        if current_found >= result_number then
          return result
        end
      end
    end
  end

  return result
end

local function dedent(str, leave_indent)
  -- find minimum common indent across lines
  local indent = nil
  for line in str:gmatch('[^\n]+') do
    local line_indent = line:match('^%s+') or ''
    if indent == nil or #line_indent < #indent then
      indent = line_indent
    end
  end
  if indent == nil or #indent == 0 then
    -- no minimum common indent
    return str
  end
  local left_indent = (' '):rep(leave_indent or 0)
  -- create a pattern for the indent
  indent = indent:gsub('%s', '[ \t]')
  -- strip it from the first line
  str = str:gsub('^'..indent, left_indent)
  -- strip it from the remaining lines
  str = str:gsub('[\n]'..indent, '\n' .. left_indent)
  return str
end

local make_vim9script = function(text)
  local generator = require('vim9jit.generator')
  local fmt = generator._utils.fmt

  return 'vim9script\n' .. fmt(text)
end

local execute = function(vim9_str)
  local generator = require('vim9jit.generator')
  local generate = generator.generate

  local compiled = generate(make_vim9script(vim9_str))
  local loaded = loadstring(compiled .. "\nreturn RESULT")

  local ok, result = pcall(loaded)
  if not ok then
    print("GENERATED:", compiled)
    print("   FROM  :", make_vim9script(vim9_str))
    print("LOADED FAILURE", loaded)
  end

  return result, compiled
end

local assert_execute = function(expected, vim9_str)
  local result, compiled = execute(vim9_str)

  local ok = true
  if type(expected) == 'function' then
    ok, expected = pcall(expected)
  end

  if not ok or not pcall(eq, expected, result) then
    print(string.rep("=", 80))
    print("vim9_str:", vim9_str)
    print("COMPILED:", tostring(compiled))
    eq(expected, result)
  end
end

local has_no_errors = function(str)
  assert_execute(true, str .. "\nvar RESULT = true")
  eq({}, vim.v.errors)
end

return {
  eq             = eq,
  neq            = neq,
  get_first_item = get_first_item,
  get_item       = get_item,
  dedent         = dedent,
  assert_execute = assert_execute,
  has_no_errors  = has_no_errors,
}
