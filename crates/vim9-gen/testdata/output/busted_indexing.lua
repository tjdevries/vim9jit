local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  local l = { 1, 2, 3 }

  Test_can_index = function()
    NVIM9.fn["assert_equal"](1, NVIM9.index(l, 0))
  end

  Test_minus = function()
    local index = 1
    NVIM9.fn["assert_equal"](1, NVIM9.index(l, NVIM9.ops["Minus"](index, 1)))
  end

  Test_both = function()
    NVIM9.fn["assert_equal"]({ 1, 2 }, NVIM9.slice(l, 0, 1))
  end

  Test_left = function()
    NVIM9.fn["assert_equal"]({ 2, 3 }, NVIM9.slice(l, 1, nil))
  end

  Test_right = function()
    NVIM9.fn["assert_equal"]({ 1, 2 }, NVIM9.slice(l, nil, 1))
  end

  Test_index_with_prefix_spaced = function()
    NVIM9.fn["assert_equal"]({ 3 }, NVIM9.slice(l, -1, nil))
  end

  Test_index_with_prefix = function()
    NVIM9.fn["assert_equal"]({ 3 }, NVIM9.slice(l, -1, nil))
  end

  Test_string = function()
    local foo = "abcd"
    NVIM9.fn["assert_equal"](NVIM9.slice(foo, nil, -2), "abc")
  end
end)
