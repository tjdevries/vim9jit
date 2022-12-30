local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_operations = function()
    local bool5 = NVIM9.convert.decl_bool(NVIM9.ops["And"](1, true))
    NVIM9.fn["assert_equal"](true, bool5)
    local bool6 = NVIM9.convert.decl_bool(NVIM9.ops["And"](0, 1))
    NVIM9.fn["assert_equal"](false, bool6)
    local bool7 = NVIM9.convert.decl_bool(NVIM9.ops["Or"](0, NVIM9.ops["And"](1, true)))
    NVIM9.fn["assert_equal"](true, bool7)
  end

  Test_lsp_from_yega = function()
    local x = true
    if NVIM9.bool(NVIM9.ops["Or"](NVIM9.prefix["Bang"](NVIM9.fn["has"]("vim9script")), 900 < 900)) then
      x = false
    end

    NVIM9.fn["assert_equal"](true, x)
  end
end)
