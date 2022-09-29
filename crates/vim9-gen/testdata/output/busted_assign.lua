describe("filename", function()
	-- vim9script

	it("Test_assignment_bool", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local bool1 = require("vim9script").convert.decl_bool(true)
		vim.fn["assert_equal"](vim.v["true"], bool1)
		local bool2 = require("vim9script").convert.decl_bool(false)
		vim.fn["assert_equal"](vim.v["false"], bool2)

		-- Token(EndOfLine, "\n", (7,0)->(7,0))
		local bool3 = require("vim9script").convert.decl_bool(0)
		vim.fn["assert_equal"](false, bool3)
		local bool4 = require("vim9script").convert.decl_bool(1)
		vim.fn["assert_equal"](true, bool4)

		-- Token(EndOfLine, "\n", (12,0)->(12,0))
		local bool5 = require("vim9script").convert.decl_bool(require("vim9script").ops["And"](1, true))
		vim.fn["assert_equal"](true, bool5)
		local bool6 = require("vim9script").convert.decl_bool(require("vim9script").ops["And"](0, 1))
		vim.fn["assert_equal"](false, bool6)
		local bool7 = require("vim9script").convert.decl_bool(
			require("vim9script").ops["And"](require("vim9script").ops["Or"](0, 1), true)
		)
		vim.fn["assert_equal"](true, bool7)

		-- Token(EndOfLine, "\n", (19,0)->(19,0))

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

		-- Token(EndOfLine, "\n", (36,0)->(36,0))

		-- #   var cp: bool = &cp

		-- #   var fen: bool = &l:fen

		-- # END

		-- # v9.CheckScriptSuccess(lines)

		-- # v9.CheckDefAndScriptFailure(['var x: bool = 2'], 'E1012:')

		-- # v9.CheckDefAndScriptFailure(['var x: bool = -1'], 'E1012:')

		-- # v9.CheckDefAndScriptFailure(['var x: bool = [1]'], 'E1012:')

		-- # v9.CheckDefAndScriptFailure(['var x: bool = {}'], 'E1012:')

		-- # v9.CheckDefAndScriptFailure(['var x: bool = "x"'], 'E1012:')

		-- Token(EndOfLine, "\n", (46,0)->(46,0))

		-- # v9.CheckDefAndScriptFailure(['var x: bool = "x"', '', 'eval 0'], 'E1012:', 1)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)
end)
