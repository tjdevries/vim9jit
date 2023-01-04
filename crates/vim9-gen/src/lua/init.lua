local M = {}

M.ternary = function(cond, if_true, if_false)
  if cond then
    if type(if_true) == 'function' then
      return if_true()
    else
      return if_true
    end
  else
    if type(if_false) == 'function' then
      return if_false()
    else
      return if_false
    end
  end
end

M.fn_mut = function(name, args, info)
  local result = vim.fn._Vim9ScriptFn(name, args)
  for idx, val in pairs(result[2]) do
    M.replace(args[idx], val)
  end

  -- Substitute returning the reference to the
  -- returned value
  if info.replace then
    return args[info.replace + 1]
  end

  return result[1]
end

M.replace = function(orig, new)
  if type(orig) == 'table' and type(new) == 'table' then
    for k in pairs(orig) do
      orig[k] = nil
    end

    for k, v in pairs(new) do
      orig[k] = v
    end

    return orig
  end

  return new
end

M.index = function(obj, idx)
  if vim.tbl_islist(obj) then
    if idx < 0 then
      return obj[#obj + idx + 1]
    else
      return obj[idx + 1]
    end
  elseif type(obj) == 'table' then
    return obj[idx]
  elseif type(obj) == 'string' then
    return string.sub(obj, idx + 1, idx + 1)
  end

  error('invalid type for indexing: ' .. vim.inspect(obj))
end

M.index_expr = function(idx)
  if type(idx) == 'string' then
    return idx
  elseif type(idx) == 'number' then
    return idx + 1
  else
    error(string.format('not yet handled: %s', vim.inspect(idx)))
  end
end

M.slice = function(obj, start, finish)
  if start == nil then
    start = 0
  end

  if start < 0 then
    start = #obj + start
  end
  assert(type(start) == 'number')

  if finish == nil then
    finish = #obj
  end

  if finish < 0 then
    finish = #obj + finish
  end
  assert(type(finish) == 'number')

  local slicer
  if vim.tbl_islist(obj) then
    slicer = vim.list_slice
  elseif type(obj) == 'string' then
    slicer = string.sub
  else
    error('invalid type for slicing: ' .. vim.inspect(obj))
  end

  return slicer(obj, start + 1, finish + 1)
end

-- Currently unused, but this could be used to embed vim9jit within a
-- running nvim application and transpile "on the fly" as files are
-- sourced. There would still need to be some work done to make that
-- work correctly with imports and what not, but overall it could
-- work well for calling ":source X" from within a vimscript/vim9script
-- function
M.make_source_cmd = function()
  local group = vim.api.nvim_create_augroup('vim9script-source', {})
  vim.api.nvim_create_autocmd('SourceCmd', {
    pattern = '*.vim',
    group = group,
    callback = function(a)
      local file = vim.fn.readfile(a.file)
      for _, line in ipairs(file) do
        -- TODO: Or starts with def <something>
        --  You can use def in legacy vim files
        if vim.startswith(line, 'vim9script') then
          -- TODO: Use the rust lib to actually
          -- generate the corresponding lua code and then
          -- execute that (instead of sourcing it directly)
          return
        end
      end

      vim.api.nvim_exec(table.concat(file, '\n'), false)
    end,
  })
end

M.iter = function(expr)
  if vim.tbl_islist(expr) then
    return ipairs(expr)
  else
    return pairs(expr)
  end
end

M.ITER_DEFAULT = 0
M.ITER_CONTINUE = 1
M.ITER_BREAK = 2
M.ITER_RETURN = 3

return M
