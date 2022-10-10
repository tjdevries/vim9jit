local fn = {}

fn.insert = function(list, item, idx)
  if idx == nil then
    idx = 1
  end

  table.insert(list, idx + 1, item)

  return list
end

fn.popup_menu = function(what, options)
  vim.cmd.vnew()
  return vim.api.nvim_get_current_win()
end

fn.prop_type_add = function(...)
  local args = { ... }
  print("[prop_type_add]", vim.inspect(args))
end

do
  local patch_overrides = {
    -- Include some vim patches that I don't care about
    [ [[patch-8.2.2261]] ] = true,
  }

  fn.has = function(patch)
    if patch_overrides[patch] then
      return true
    end

    return vim.fn.has(patch)
  end
end

fn = setmetatable(fn, {
  __index = vim.fn,
})

return fn
