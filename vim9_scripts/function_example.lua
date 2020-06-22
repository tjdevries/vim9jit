--[=[
Original vimscript
vim9script

def ExampleFunc()
  let x_thing: number = 5
  g:vim_global_var = 12
  b:vim_global_var = 12
enddef
--]=]

local function ExampleFunc()
  local x_thing = 5
  vim.g["vim_global_var"] = 12
  vim_global_var = 12
end
