local imported = {}
imported.autoload = setmetatable({}, {
  __index = function(_, name)
    local luaname = 'autoload/' .. string.gsub(name, '%.vim$', '.lua')
    local runtime_file = vim.api.nvim_get_runtime_file(luaname, false)[1]
    if not runtime_file then
      error('unable to find autoload file:' .. name)
    end

    return imported.absolute[vim.fn.fnamemodify(runtime_file, ':p')]
  end,
})

imported.absolute = setmetatable({}, {
  __index = function(self, name)
    if vim.loop.fs_stat(name) then
      local result = loadfile(name)()
      rawset(self, name, result)

      return result
    end

    error(string.format('unabled to find absolute file: %s', name))
  end,
})

return function(info)
  local name = info.name

  if info.autoload then
    return imported.autoload[info.name]
  end

  local debug_info = debug.getinfo(2, 'S')
  local sourcing_path = vim.fn.fnamemodify(string.sub(debug_info.source, 2), ':p')

  -- Relative paths
  if vim.startswith(name, '../') or vim.startswith(name, './') then
    local luaname = string.gsub(name, '%.vim$', '.lua')
    local directory = vim.fn.fnamemodify(sourcing_path, ':h')
    local search = directory .. '/' .. luaname
    return imported.absolute[search]
  end

  if vim.startswith(name, '/') then
    error('absolute path')
    -- local luaname = string.gsub(name, "%.vim", ".lua")
    -- local runtime_file = vim.api.nvim_get_runtime_file(luaname, false)[1]
    -- if runtime_file then
    --   runtime_file = vim.fn.fnamemodify(runtime_file, ":p")
    --   return loadfile(runtime_file)()
    -- end
  end

  error('Unhandled case' .. vim.inspect(info) .. vim.inspect(debug_info))
end
