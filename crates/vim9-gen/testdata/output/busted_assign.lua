local NVIM9 = require("vim9script")
local __VIM9_MODULE = {}
describe("filename", function()
  local Test_assignment_bool_1 = nil
  local Test_assignment_bool_2 = nil
  local Test_assignment_bool_3 = nil
  local Test_unpacked_identifiers = nil
  local Test_modifier_prefixes = nil
  -- vim9script

  it("Test_assignment_bool_1", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool1 = NVIM9.convert.decl_bool(true)
    NVIM9.fn["assert_equal"](vim.v["true"], bool1)
    local bool2 = NVIM9.convert.decl_bool(false)
    NVIM9.fn["assert_equal"](vim.v["false"], bool2)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_assignment_bool_2", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool3 = NVIM9.convert.decl_bool(0)
    NVIM9.fn["assert_equal"](false, bool3)
    local bool4 = NVIM9.convert.decl_bool(1)
    NVIM9.fn["assert_equal"](true, bool4)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_assignment_bool_3", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local bool5 = NVIM9.convert.decl_bool(NVIM9.ops["And"](1, true))
    NVIM9.fn["assert_equal"](true, bool5)
    local bool6 = NVIM9.convert.decl_bool(NVIM9.ops["And"](0, 1))
    NVIM9.fn["assert_equal"](false, bool6)
    local bool7 = NVIM9.convert.decl_bool(NVIM9.ops["Or"](0, NVIM9.ops["And"](1, true)))
    NVIM9.fn["assert_equal"](true, bool7)

    -- # var lines =<< trim END
    -- #   vim9script
    -- #   def GetFlag(): bool
    -- #     var flag: bool = 1
    -- #     return flag
    -- #   enddef
    -- #   var flag: bool = GetFlag()
    -- #   assert_equal(true, flag)
    -- #   flag = 0
    -- #   assert_equal(false, flag)
    -- #   flag = 1
    -- #   assert_equal(true, flag)
    -- #   flag = 1 || true
    -- #   assert_equal(true, flag)
    -- #   flag = 1 && false
    -- #   assert_equal(false, flag)

    -- #   var cp: bool = &cp
    -- #   var fen: bool = &l:fen
    -- # END
    -- # v9.CheckScriptSuccess(lines)
    -- # v9.CheckDefAndScriptFailure(['var x: bool = 2'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = -1'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = [1]'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = {}'], 'E1012:')
    -- # v9.CheckDefAndScriptFailure(['var x: bool = "x"'], 'E1012:')

    -- # v9.CheckDefAndScriptFailure(['var x: bool = "x"', '', 'eval 0'], 'E1012:', 1)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_unpacked_identifiers", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x, y = unpack({ 1, 2 })
    NVIM9.fn["assert_equal"](1, x)
    NVIM9.fn["assert_equal"](2, y)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_modifier_prefixes", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local foo = 10
    foo = foo - 1
    NVIM9.fn["assert_equal"](9, foo)

    foo = foo + 1
    foo = foo + 1
    NVIM9.fn["assert_equal"](11, foo)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)
end)
return __VIM9_MODULE
