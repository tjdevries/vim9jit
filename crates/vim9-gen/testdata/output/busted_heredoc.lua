describe("filename", function()
	-- vim9script

	it("Test_simple_heredoc", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = { [==[hello]==], [==[world]==] }
		-- Token(EndOfLine, "\n", (7,0)->(7,0))
		vim.fn.assert_equal({ "hello", "world" }, x)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (10,0)->(10,0))

	it("Test_simple_heredoc_with_whitespace", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = { [==[    hello]==], [==[  world]==] }
		-- Token(EndOfLine, "\n", (16,0)->(16,0))
		vim.fn.assert_equal({ "    hello", "  world" }, x)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (19,0)->(19,0))
	-- # def Test_simple_heredoc_with_quotes()
	-- #   var x =<< END
	-- #     "hello"
	-- #   world
	-- #   END
	-- #
	-- #   assert_equal(["    "hello"", "  world"], x)
	-- # enddef
end)
