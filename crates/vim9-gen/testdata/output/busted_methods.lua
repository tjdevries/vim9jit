local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_string_methods = nil
  local Test_list_methods = nil
  local Test_inplace = nil
  local Test_inplace_inplace_multi = nil
  local Test_inplace_inplace_single = nil
  local Test_method_precedence = nil
  -- vim9script

  it("Test_string_methods", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local s = "hello"
    NVIM9.fn["assert_equal"](NVIM9.fn["len"](s), 5)

    NVIM9.fn["assert_equal"](NVIM9.fn["len"]("hello"), 5)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_list_methods", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = NVIM9.fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 })

    NVIM9.fn["assert_equal"]({ 1, 12, 2, 4, 5, 7, 8 }, x)

    local numeric = NVIM9.fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 }, "n")

    NVIM9.fn["assert_equal"]({ 1, 2, 4, 5, 7, 8, 12 }, numeric)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_inplace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 4, 2, 5 }
    NVIM9.fn_mut("sort", { x }, { replace = 0 })

    NVIM9.fn["assert_equal"]({ 1, 2, 4, 5 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_inplace_inplace_multi", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { 1, 4, 2, 5 }
    NVIM9.fn_mut("sort", { x }, { replace = 0 })
    NVIM9.fn_mut(
      "filter",
      { x, function(_, y)
        return NVIM9.ops["EqualTo"](NVIM9.ops["Modulo"](y, 2), 0)
      end },
      { replace = 0 }
    )
    NVIM9.fn["assert_equal"]({ 2, 4 }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_inplace_inplace_single", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    NVIM9.fn_mut(
      "filter",
      {
        NVIM9.fn_mut("sort", { foo }, { replace = 0 }),
        function(_, y)
          return NVIM9.ops["EqualTo"](NVIM9.ops["Modulo"](y, 2), 0)
        end,
      },
      { replace = 0 }
    )

    NVIM9.fn["assert_equal"]({ 2, 4 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_method_precedence", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local server = { ["filetype"] = true }
    NVIM9.fn["assert_equal"](false, NVIM9.prefix["Bang"](NVIM9.fn["has_key"](server, "filetype")))

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
