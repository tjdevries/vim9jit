local generator = require "vim9jit.generator"
local grammar_def = require("vim9jit.parser").grammar_def

local transpiler = {}

transpiler.transpile = function(vim_source)
  vim_source = vim.trim(vim_source)

  local vim_lines = vim.split(vim_source, "\n")
  for _, line in ipairs(vim_lines) do
    if line ~= "" then
      -- TODO: probably could have comments above this line?
      if string.find(line, "%s*vim9script") then
        return transpiler._transpile_vim9(vim_source)
      else
        return transpiler._transpile_embedded(vim_source)
      end
    end
  end
end

local wrap_with_lua_eof = function(lua_str)
  return string.format(vim.trim [[
lua << EOF
%s
EOF
  ]], lua_str)
end

transpiler._transpile_vim9 = function(vim_source)
  local output = generator.generate(vim_source, false)

  return wrap_with_lua_eof(output)
end

transpiler._transpile_embedded = function(vim_source)
  local vim_lines = vim.split(vim_source, "\n")

  local result = {}
  local current_def = {}

  local def_level = 0
  for _, line in ipairs(vim_lines) do
    if def_level == 0 then
      if not string.find(line, "^%s*def ") then
        table.insert(result, line)
      else
        def_level = def_level + 1
        table.insert(current_def, line)
      end

      assert(not string.find(line, "^%s*enddef"))
    else
      table.insert(current_def, line)

      if string.find(line, "^%s*enddef") then
        def_level = def_level - 1

        if def_level == 0 then
          local old_grammar = generator._utils.get_grammar()
          generator._utils.set_grammar(grammar_def)

          table.insert(result, wrap_with_lua_eof(generator.generate_def(table.concat(current_def, "\n"))))

          generator._utils.set_grammar(old_grammar)
        end
      end
    end
  end

  return table.concat(result, "\n")
end

return transpiler
