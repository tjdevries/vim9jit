local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_string_methods = function()
    local s = "hello"
    NVIM9.fn["assert_equal"](NVIM9.fn["len"](s), 5)

    NVIM9.fn["assert_equal"](NVIM9.fn["len"]("hello"), 5)
  end

  Test_list_methods = function()
    local x = NVIM9.fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 })

    NVIM9.fn["assert_equal"]({ 1, 12, 2, 4, 5, 7, 8 }, x)

    local numeric = NVIM9.fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 }, "n")

    NVIM9.fn["assert_equal"]({ 1, 2, 4, 5, 7, 8, 12 }, numeric)
  end

  Test_inplace = function()
    local x = { 1, 4, 2, 5 }
    NVIM9.fn_mut("sort", { x }, { replace = 0 })

    NVIM9.fn["assert_equal"]({ 1, 2, 4, 5 }, x)
  end

  Test_inplace_inplace_multi = function()
    local x = { 1, 4, 2, 5 }
    NVIM9.fn_mut("sort", { x }, { replace = 0 })
    NVIM9.fn_mut("filter", {
      x,
      function(_, y)
        return NVIM9.ops["Modulo"](y, 2) == 0
      end,
    }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 2, 4 }, x)
  end

  Test_inplace_inplace_single = function()
    local foo = { 1, 4, 2, 5 }
    NVIM9.fn_mut(
      "filter",
      {
        NVIM9.fn_mut("sort", { foo }, { replace = 0 }),
        function(_, y)
          return NVIM9.ops["Modulo"](y, 2) == 0
        end,
      },
      { replace = 0 }
    )

    NVIM9.fn["assert_equal"]({ 2, 4 }, foo)
  end

  Test_method_precedence = function()
    local server = { ["filetype"] = true }
    NVIM9.fn["assert_equal"](false, NVIM9.prefix["Bang"](NVIM9.fn["has_key"](server, "filetype")))
  end
end)
