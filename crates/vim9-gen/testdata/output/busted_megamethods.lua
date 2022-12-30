local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_inplace_discarded = function()
    NVIM9.fn["assert_equal"](
      { 2, 4 },
      NVIM9.fn["filter"](
        NVIM9.fn["sort"]({ 1, 4, 2, 5 }),
        function(_, y)
          return NVIM9.ops["Modulo"](y, 2) == 0
        end
      )
    )

    local foo = NVIM9.fn["filter"](
      NVIM9.fn["sort"]({ 1, 4, 2, 5 }),
      function(_, y)
        return NVIM9.ops["Modulo"](y, 2) == 0
      end
    )
    NVIM9.fn["assert_equal"]({ 2, 4 }, foo)
  end

  Test_returned_foo = function()
    local foo = { 1, 4, 2, 5 }
    NVIM9.fn["assert_equal"](
      { 5, 4, 2, 1 },
      NVIM9.fn_mut("reverse", { NVIM9.fn_mut("sort", { foo }, { replace = 0 }) }, { replace = 0 })
    )
  end

  Test_returned_foo = function()
    local foo = { 1, 4, 2, 5 }
    local bar = foo
    NVIM9.fn_mut("reverse", { NVIM9.fn_mut("sort", { foo }, { replace = 0 }) }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 5, 4, 2, 1 }, foo)
    NVIM9.fn["assert_equal"]({ 5, 4, 2, 1 }, bar)
    NVIM9.fn["assert_equal"](foo, bar)
  end
end)
