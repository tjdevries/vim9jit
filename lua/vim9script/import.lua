local imported = {
  autoload = setmetatable({}, {
    __index = function(self, name)
      local luaname = "autoload/" .. string.gsub(name, "%.vim$", ".lua")
      local runtime_file = vim.api.nvim_get_runtime_file(luaname, false)[1]
      if not runtime_file then
        error("unable to find autoload file:" .. name)
      end

      local result = loadfile(runtime_file)()
      rawset(self, name, result)

      return result
    end,
  }),
}

return function(info)
  local name = info.name

  if info.autoload then
    return imported.autoload[info.name]
  end

  if not (vim.startswith(name, "../") or vim.startswith(name, "./") or vim.startswith(name, "/")) then
    local luaname = string.gsub(name, "%.vim", ".lua")
    local runtime_file = vim.api.nvim_get_runtime_file(luaname, false)[1]
    if runtime_file then
      runtime_file = vim.fn.fnamemodify(runtime_file, ":p")
      return loadfile(runtime_file)()
    end
  end

  local debug_info = debug.getinfo(2, "S")
  error("Unhandled case" .. vim.inspect(info) .. vim.inspect(debug_info))
end
