describe("filename", function()
	-- vim9script

	it("Test_simple_heredoc", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = { [==[hello]==], [==[world]==] }
		-- Token(EndOfLine, "\n", (7,0)->(7,0))
		vim.fn["assert_equal"]({ "hello", "world" }, x)

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
		vim.fn["assert_equal"]({ "    hello", "  world" }, x)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (19,0)->(19,0))

	it("Test_simple_heredoc_with_no_whitespace_trim", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = require("vim9script").heredoc.trim({ [==[    hello]==], [==[world]==] })
		-- Token(EndOfLine, "\n", (25,0)->(25,0))
		vim.fn["assert_equal"]({ "    hello", "world" }, x)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (28,0)->(28,0))

	it("Test_simple_heredoc_with_whitespace_trim", function()
		-- Set errors to empty
		vim.v.errors = {}

		-- Actual test
		local x = require("vim9script").heredoc.trim({ [==[        hello]==], [==[          world]==] })
		-- Token(EndOfLine, "\n", (34,0)->(34,0))
		vim.fn["assert_equal"]({ "hello", "  world" }, x)

		-- Assert that errors is still empty
		assert.are.same({}, vim.v.errors)
	end)

	-- Token(EndOfLine, "\n", (37,0)->(37,0))
	-- # def Test_simple_heredoc_with_quotes()
	-- #   var x =<< END
	-- #     "hello"
	-- #   world
	-- #   END
	-- #
	-- #   assert_equal(["    "hello"", "  world"], x)
	-- # enddef
end)
