local NVIM9 = require("_vim9script")
describe("filename", function()
  Test_method_comments = function()
    local x = { 1, 2, 3 }
    NVIM9.fn_mut("filter", { x, function(_, y)
      return y == 2
    end }, { replace = 0 })

    NVIM9.fn["assert_equal"](x, { 2 })
  end
end)
