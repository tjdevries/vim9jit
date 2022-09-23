local M = {}

M.ops = require "vim9script.ops"
M.convert = require "vim9script.convert"
M.heredoc = require "vim9script.heredoc"

M.slice = function(tbl, start, finish)
  assert(vim.tbl_islist(tbl), "only can use slices on lists")
  if start == nil then
    start = 0
  end
  assert(type(start) == "number")

  if finish == nil then
    finish = #tbl
  end
  assert(type(finish) == "number")

  return vim.list_slice(tbl, start + 1, finish + 1)
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
