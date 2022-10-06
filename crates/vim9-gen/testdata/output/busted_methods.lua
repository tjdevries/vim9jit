local __VIM9_MODULE = {}
describe("filename", function()
  -- vim9script

  it("Test_string_methods", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local s = "hello"
    require("vim9script").fn["assert_equal"](require("vim9script").fn["len"](s), 5)

    -- Token(EndOfLine, "\n", (5,0)->(5,0))
    require("vim9script").fn["assert_equal"](require("vim9script").fn["len"]("hello"), 5)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (8,0)->(8,0))

  -- Token(EndOfLine, "\n", (9,0)->(9,0))

  it("Test_list_methods", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = require("vim9script").fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 })

    -- Token(EndOfLine, "\n", (13,0)->(13,0))
    require("vim9script").fn["assert_equal"]({ 1, 12, 2, 4, 5, 7, 8 }, x)

    -- Token(EndOfLine, "\n", (15,0)->(15,0))
    local numeric = require("vim9script").fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 }, "n")

    -- Token(EndOfLine, "\n", (18,0)->(18,0))
    require("vim9script").fn["assert_equal"]({ 1, 2, 4, 5, 7, 8, 12 }, numeric)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (21,0)->(21,0))

  it("Test_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 4, 2, 5 }
    require("vim9script").fn_mut("sort", { x }, { replace = 0 })

    -- Token(EndOfLine, "\n", (25,0)->(25,0))
    require("vim9script").fn["assert_equal"]({ 1, 2, 4, 5 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (28,0)->(28,0))

  it("Test_inplace_inplace_multi", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 4, 2, 5 }
    require("vim9script").fn_mut("sort", { x }, { replace = 0 })
    require("vim9script").fn_mut(
      "filter",
      {
        x,
        function(_, y)
          return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
        end,
      },
      { replace = 0 }
    )
    require("vim9script").fn["assert_equal"]({ 2, 4 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (35,0)->(35,0))

  -- Token(EndOfLine, "\n", (36,0)->(36,0))

  it("Test_inplace_inplace_single", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    require("vim9script").fn_mut(
      "filter",
      {
        require("vim9script").fn_mut("sort", { foo }, { replace = 0 }),
        function(_, y)
          return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
        end,
      },
      { replace = 0 }
    )

    -- Token(EndOfLine, "\n", (40,0)->(40,0))
    require("vim9script").fn["assert_equal"]({ 2, 4 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
