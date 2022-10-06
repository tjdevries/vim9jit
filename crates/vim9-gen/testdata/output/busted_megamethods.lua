local __VIM9_MODULE = {}
describe("filename", function()
  -- vim9script

  it("Test_inplace_discarded", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    require("vim9script").fn["assert_equal"](
      { 2, 4 },
      require("vim9script").fn["filter"](
        require("vim9script").fn["sort"]({ 1, 4, 2, 5 }),
        function(_, y)
          return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
        end
      )
    )

    -- Token(EndOfLine, "\n", (4,0)->(4,0))
    local foo = require("vim9script").fn["filter"](
      require("vim9script").fn["sort"]({ 1, 4, 2, 5 }),
      function(_, y)
        return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
      end
    )
    require("vim9script").fn["assert_equal"]({ 2, 4 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (8,0)->(8,0))

  it("Test_returned_foo", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    require("vim9script").fn["assert_equal"](
      { 5, 4, 2, 1 },
      require("vim9script").fn_mut(
        "reverse",
        { require("vim9script").fn_mut("sort", { foo }, { replace = 0 }) },
        { replace = 0 }
      )
    )

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (13,0)->(13,0))

  it("Test_returned_foo", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    local bar = foo
    require("vim9script").fn_mut(
      "reverse",
      { require("vim9script").fn_mut("sort", { foo }, { replace = 0 }) },
      { replace = 0 }
    )
    require("vim9script").fn["assert_equal"]({ 5, 4, 2, 1 }, foo)
    require("vim9script").fn["assert_equal"]({ 5, 4, 2, 1 }, bar)
    require("vim9script").fn["assert_equal"](foo, bar)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
