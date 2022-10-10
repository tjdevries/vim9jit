local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_inplace_discarded = nil
  local Test_returned_foo = nil
  local Test_returned_foo = nil
  -- vim9script

  it("Test_inplace_discarded", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    NVIM9.fn["assert_equal"](
      { 2, 4 },
      NVIM9.fn["filter"](
        NVIM9.fn["sort"]({ 1, 4, 2, 5 }),
        function(_, y)
          return NVIM9.ops["EqualTo"](NVIM9.ops["Modulo"](y, 2), 0)
        end
      )
    )

    local foo = NVIM9.fn["filter"](
      NVIM9.fn["sort"]({ 1, 4, 2, 5 }),
      function(_, y)
        return NVIM9.ops["EqualTo"](NVIM9.ops["Modulo"](y, 2), 0)
      end
    )
    NVIM9.fn["assert_equal"]({ 2, 4 }, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_returned_foo", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    NVIM9.fn["assert_equal"](
      { 5, 4, 2, 1 },
      NVIM9.fn_mut("reverse", { NVIM9.fn_mut("sort", { foo }, { replace = 0 }) }, { replace = 0 })
    )

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_returned_foo", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = { 1, 4, 2, 5 }
    local bar = foo
    NVIM9.fn_mut("reverse", { NVIM9.fn_mut("sort", { foo }, { replace = 0 }) }, { replace = 0 })
    NVIM9.fn["assert_equal"]({ 5, 4, 2, 1 }, foo)
    NVIM9.fn["assert_equal"]({ 5, 4, 2, 1 }, bar)
    NVIM9.fn["assert_equal"](foo, bar)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
