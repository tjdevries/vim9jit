describe("filename", function()
	-- vim9script

	it("Test_string_methods", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local s = "hello"
		vim.fn["assert_equal"](vim.fn["len"](s), 5)

		-- Token(EndOfLine, "\n", (5,0)->(5,0))
		vim.fn["assert_equal"](vim.fn["len"]("hello"), 5)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (8,0)->(8,0))

	-- Token(EndOfLine, "\n", (9,0)->(9,0))

	it("Test_list_methods", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = vim.fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 })

		-- Token(EndOfLine, "\n", (13,0)->(13,0))
		vim.fn["assert_equal"]({ 1, 12, 2, 4, 5, 7, 8 }, x)

		-- Token(EndOfLine, "\n", (15,0)->(15,0))
		local numeric = vim.fn["sort"]({ 5, 4, 2, 1, 7, 12, 8 }, "n")

		-- Token(EndOfLine, "\n", (18,0)->(18,0))
		vim.fn["assert_equal"]({ 1, 2, 4, 5, 7, 8, 12 }, numeric)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (21,0)->(21,0))

	it("Test_inplace", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = { 1, 4, 2, 5 };

		(function()
			local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { x })
			x = __vim9_result[2][1]
			return __vim9_result[1]
		end)()

		-- Token(EndOfLine, "\n", (25,0)->(25,0))
		vim.fn["assert_equal"]({ 1, 2, 4, 5 }, x)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (28,0)->(28,0))

	it("Test_inplace_inplace", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = { 1, 4, 2, 5 }
		vim.fn["filter"](
			(function()
				local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { x })
				x = __vim9_result[2][1]
				return __vim9_result[1]
			end)(),
			function(_, y)
				return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
			end
		)
		vim.fn["assert_equal"]({ 2, 4 }, x)

		-- Token(EndOfLine, "\n", (33,0)->(33,0))
		local foo = { 1, 4, 2, 5 }
		foo = vim.fn["filter"](
			(function()
				local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { foo })
				foo = __vim9_result[2][1]
				return __vim9_result[1]
			end)(),
			function(_, y)
				return require("vim9script").ops["EqualTo"](require("vim9script").ops["Modulo"](y, 2), 0)
			end
		)

		-- Token(EndOfLine, "\n", (36,0)->(36,0))
		vim.fn["assert_equal"]({ 2, 4 }, foo)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (39,0)->(39,0))

	-- #->filter((_, x) => x % 2 == 0)

	-- #->map((_, y) => y + 1)

	-- #->sort()

	-- Token(EndOfLine, "\n", (43,0)->(43,0))

	-- # var expr_prec = -1.234->string()

	-- #

	-- # var foo = base->name(args)

	-- # var foo = base->some.name(args)

	-- # var foo = base->alist[idx](args)

	-- # var foo = base->(getFuncRef())(args)
end)
