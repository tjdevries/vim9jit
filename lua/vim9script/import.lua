local imported = {}

return function(info)
  local name = info.name

  if not (vim.startswith(name, "../") or vim.startswith(name, "./") or vim.startswith(name, "/")) then
    local luaname = string.gsub(name, "%.vim", ".lua")
    if info.autoload then
      luaname = "autoload/" .. luaname
    end

    local runtime_file = vim.api.nvim_get_runtime_file(luaname, false)[1]
    print("RUNTIME FILE:", luaname, runtime_file)
  end

  local info = debug.getinfo(2, "S")
end
