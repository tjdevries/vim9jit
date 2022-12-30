local NVIM9 = require("_vim9script")
describe("filename", function()
  -- vim9script

  Test_simple_heredoc = function()
    local x = { [==[hello]==], [==[world]==] }

    NVIM9.fn["assert_equal"]({ "hello", "world" }, x)
  end

  Test_simple_heredoc_with_whitespace = function()
    local x = { [==[    hello]==], [==[  world]==] }

    NVIM9.fn["assert_equal"]({ "    hello", "  world" }, x)
  end

  Test_simple_heredoc_with_no_whitespace_trim = function()
    local x = NVIM9.heredoc.trim({ [==[    hello]==], [==[world]==] })

    NVIM9.fn["assert_equal"]({ "    hello", "world" }, x)
  end

  Test_simple_heredoc_with_whitespace_trim = function()
    local x = NVIM9.heredoc.trim({ [==[        hello]==], [==[          world]==] })

    NVIM9.fn["assert_equal"]({ "hello", "  world" }, x)
  end

  -- # def Test_simple_heredoc_with_quotes()
  -- #   var x =<< END
  -- #     "hello"
  -- #   world
  -- #   END
  -- #
  -- #   assert_equal(["    "hello"", "  world"], x)
  -- # enddef
end)
