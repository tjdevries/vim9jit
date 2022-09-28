local M = {}

M.ops = require "vim9script.ops"
M.prefix = require "vim9script.prefix"
M.convert = require "vim9script.convert"
M.heredoc = require "vim9script.heredoc"

M.slice = function(obj, start, finish)
  local slicer
  if vim.tbl_islist(obj) then
    slicer = vim.list_slice
  elseif type(obj) == "string" then
    slicer = string.sub
  else
    error("invalid type for slicing: " .. vim.inspect(obj))
  end

  if start == nil then
    start = 0
  end

  if start < 0 then
    start = #obj - start
  end
  assert(type(start) == "number")

  if finish == nil then
    finish = #obj
  end

  if finish < 0 then
    finish = #obj - finish
  end
  assert(type(finish) == "number")

  return slicer(obj, start + 1, finish + 1)
end

M.make_source_cmd = function()
  local group = vim.api.nvim_create_augroup("vim9script-source", {})
  vim.api.nvim_create_autocmd("SourceCmd", {
    pattern = "*.vim",
    group = group,
    callback = function(a)
      local file = vim.fn.readfile(a.file)
      for _, line in ipairs(file) do
        -- TODO: Or starts with def <something>
        --  You can use def in legacy vim files
        if vim.startswith(line, "vim9script") then
          -- TODO: Use the rust lib to actually
          -- generate the corresponding lua code and then
          -- execute that (instead of sourcing it directly)
          return
        end
      end

      vim.api.nvim_exec(table.concat(file, "\n"), false)
    end,
  })
end

return M
