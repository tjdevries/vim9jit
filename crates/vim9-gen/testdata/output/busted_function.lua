describe("filename", function()
  -- vim9script

  it("Test_default_args", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test

    local MyCoolFunc = function(x)
      x = vim.F.if_nil(x, 5, x)
      return x
    end

    -- Token(EndOfLine, "\n", (6,0)->(6,0))
    vim.fn["assert_equal"](MyCoolFunc(), 5)
    vim.fn["assert_equal"](MyCoolFunc(10), 10)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (10,0)->(10,0))

  it("Test_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local explicit = { 3, 2, 1 }
    explicit = require("vim9script").fn_mut("sort", { explicit }, { replace = 0 })
    vim.fn["assert_equal"]({ 1, 2, 3 }, explicit)

    -- Token(EndOfLine, "\n", (15,0)->(15,0))
    local inplace = { 3, 2, 1 }
    require("vim9script").fn_mut("sort", { inplace }, { replace = 0 })
    vim.fn["assert_equal"]({ 1, 2, 3 }, inplace)

    -- Token(EndOfLine, "\n", (19,0)->(19,0))
    local expr_sort = vim.fn["sort"]({ 3, 2, 1 })
    vim.fn["sort"]({ 3, 2, 1 })
    vim.fn["assert_equal"]({ 1, 2, 3 }, expr_sort)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (24,0)->(24,0))

  it("Test_insert_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 2, 3 }
    local bar = require("vim9script").fn["insert"](foo, 4, vim.fn["len"](foo))
    vim.fn["assert_equal"](foo, bar)

    -- Token(EndOfLine, "\n", (29,0)->(29,0))
    require("vim9script").fn["insert"](bar, 5, vim.fn["len"](bar))
    vim.fn["assert_equal"]({ 1, 2, 3, 4, 5 }, bar)
    vim.fn["assert_equal"](foo, bar)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (34,0)->(34,0))

  it("Test_insert_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 3, 2 }
    require("vim9script").fn_mut(
      "reverse",
      { require("vim9script").fn_mut("sort", { foo }, { replace = 0 }) },
      { replace = 0 }
    )
    vim.fn["assert_equal"]({ 3, 2, 1 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
