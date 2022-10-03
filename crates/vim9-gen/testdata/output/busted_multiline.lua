describe("filename", function()
  -- vim9script

  -- Token(EndOfLine, "\n", (2,0)->(2,0))

  it("Test_same_line", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = require("vim9script").convert.decl_bool(require("vim9script").ops["And"](true, false))

    -- Token(EndOfLine, "\n", (6,0)->(6,0))
    vim.fn["assert_equal"](x, false)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (9,0)->(9,0))

  it("Test_next_line", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local y = require("vim9script").convert.decl_bool(require("vim9script").ops["And"](true, false))

    -- Token(EndOfLine, "\n", (13,0)->(13,0))
    vim.fn["assert_equal"](y, false)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- Token(EndOfLine, "\n", (16,0)->(16,0))

  it("Test_missing_lines", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local z = require("vim9script").ops["Or"](true, false)

    -- Token(EndOfLine, "\n", (21,0)->(21,0))
    vim.fn["assert_equal"](z, true)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
