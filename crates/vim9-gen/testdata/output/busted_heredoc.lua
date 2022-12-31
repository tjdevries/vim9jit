local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  it("Test_simple_heredoc", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { [==[hello]==], [==[world]==] }

    NVIM9.fn["assert_equal"]({ "hello", "world" }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_simple_heredoc_with_whitespace", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = { [==[    hello]==], [==[  world]==] }

    NVIM9.fn["assert_equal"]({ "    hello", "  world" }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_simple_heredoc_with_no_whitespace_trim", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = NVIM9.heredoc.trim({ [==[    hello]==], [==[world]==] })

    NVIM9.fn["assert_equal"]({ "    hello", "world" }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  it("Test_simple_heredoc_with_whitespace_trim", function()
    -- Set errors to empty
    vim.v.errors = {}

    -- Actual test
    local x = NVIM9.heredoc.trim({ [==[        hello]==], [==[          world]==] })

    NVIM9.fn["assert_equal"]({ "hello", "  world" }, x)

    -- Assert that errors is still empty
    assert.are.same({}, vim.v.errors)
  end)

  -- # def Test_simple_heredoc_with_quotes()
  -- #   var x =<< END
  -- #     "hello"
  -- #   world
  -- #   END
  -- #
  -- #   assert_equal(["    "hello"", "  world"], x)
  -- # enddef
end)
