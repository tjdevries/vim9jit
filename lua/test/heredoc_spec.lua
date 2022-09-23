local heredoc = require "vim9script.heredoc"

local eq = assert.are.same

describe("heredoc", function()
  describe("trim", function()
    it("return identical list for no preceding whitespace", function()
      local doc = {
        "hello",
        "world",
      }

      eq(doc, heredoc.trim(doc))
    end)

    it("strips same whitespace for all lines", function()
      eq({ "hello", "world" }, heredoc.trim { "  hello", "  world" })
    end)

    it("strips min whitespace for all lines", function()
      eq({ "  hello", "world" }, heredoc.trim { "    hello", "  world" })
    end)
  end)
end)
