local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_same_line = function()
    local x = NVIM9.convert.decl_bool(true and false)

    NVIM9.fn["assert_equal"](x, false)
  end

  Test_next_line = function()
    local y = NVIM9.convert.decl_bool(true and false)

    NVIM9.fn["assert_equal"](y, false)
  end

  Test_missing_lines = function()
    local z = true or false

    NVIM9.fn["assert_equal"](z, true)
  end
end)
