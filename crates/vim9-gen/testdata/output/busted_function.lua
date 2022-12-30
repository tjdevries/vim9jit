local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_default_args = function()
    local MyCoolFunc = function(x)
      x = vim.F.if_nil(x, 5, x)
      return x
    end

    NVIM9.fn["assert_equal"](MyCoolFunc(), 5)
    NVIM9.fn["assert_equal"](MyCoolFunc(10), 10)
  end

  Test_inplace = function()
    local explicit = { 3, 2, 1 }
    explicit = NVIM9.fn_mut("sort", { explicit }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 1, 2, 3 }, explicit)

    local inplace = { 3, 2, 1 }
    NVIM9.fn_mut("sort", { inplace }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 1, 2, 3 }, inplace)

    local expr_sort = NVIM9.fn["sort"]({ 3, 2, 1 })
    NVIM9.fn["sort"]({ 3, 2, 1 })
    NVIM9.fn["assert_equal"]({ 1, 2, 3 }, expr_sort)
  end

  Test_insert_inplace = function()
    local foo = { 1, 2, 3 }
    local bar = NVIM9.fn["insert"](foo, 4, NVIM9.fn["len"](foo))
    NVIM9.fn["assert_equal"](foo, bar)

    NVIM9.fn["insert"](bar, 5, NVIM9.fn["len"](bar))
    NVIM9.fn["assert_equal"]({ 1, 2, 3, 4, 5 }, bar)
    NVIM9.fn["assert_equal"](foo, bar)
  end

  Test_insert_inplace = function()
    local foo = { 1, 3, 2 }
    NVIM9.fn_mut("reverse", { NVIM9.fn_mut("sort", { foo }, { replace = 0 }) }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 3, 2, 1 }, foo)
  end
end)
