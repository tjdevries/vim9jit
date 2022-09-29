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
		explicit = (function()
			local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { explicit })
			explicit = __vim9_result[2][1]
			return __vim9_result[1]
		end)()

		vim.fn["assert_equal"]({ 1, 2, 3 }, explicit)

		-- Token(EndOfLine, "\n", (15,0)->(15,0))
		local inplace = { 3, 2, 1 };

		(function()
			local __vim9_result = vim.fn["VIM9__CallUserVimlFunc"]("sort", { inplace })
			inplace = __vim9_result[2][1]
			return __vim9_result[1]
		end)()

		vim.fn["assert_equal"]({ 1, 2, 3 }, inplace)

		-- Token(EndOfLine, "\n", (19,0)->(19,0))
		local expr_sort = vim.fn["sort"]({ 3, 2, 1 })
		vim.fn["sort"]({ 3, 2, 1 })
		vim.fn["assert_equal"]({ 1, 2, 3 }, expr_sort)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)
end)
