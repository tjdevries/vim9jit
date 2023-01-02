local M = {}

M.trim = function(lines)
  local min_whitespace = 9999
  for _, line in ipairs(lines) do
    local _, finish = string.find(line, "^%s*")
    min_whitespace = math.min(min_whitespace, finish)
  end

  local trimmed_lines = {}
  for _, line in ipairs(lines) do
    table.insert(trimmed_lines, string.sub(line, min_whitespace + 1))
  end

  return trimmed_lines
end

return M
