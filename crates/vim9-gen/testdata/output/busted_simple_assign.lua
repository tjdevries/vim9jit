local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_assignment_one = function()
    local bool1 = NVIM9.convert.decl_bool(true)
    NVIM9.fn["assert_equal"](vim.v["true"], bool1)
  end
end)
