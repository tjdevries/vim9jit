
local utils = {}

function utils.dedent(str, leave_indent)
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

function utils.indent(str, indent)
  local split_str = vim.split(str, "\n")

  local indent_str = (' '):rep(indent or 2)

  local indented = {}
  for _, v in ipairs(split_str) do
    if v == '' then
      table.insert(indented, '')
    else
      table.insert(indented, indent_str .. v)
    end
  end

  return table.concat(indented, "\n")
end

return utils
