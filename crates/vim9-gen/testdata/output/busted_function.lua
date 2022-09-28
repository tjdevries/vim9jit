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
end)
