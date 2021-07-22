local types_grammar = require "vim9jit.parser.types"

local helpers = require "test.helpers"
local eq = helpers.eq
local get_item = helpers.get_item

local parsed = function(output, input, debug)
  local parsed = types_grammar:match(input)
  if debug then
    eq(nil, vim.inspect(parsed))
  end

  local items = {}
  local index = 1

  while true do
    local node = get_item(parsed, "id", "TypeSingleDefinition", index, nil, false)
    if not node then
      break
    end

    table.insert(items, node.value)
    index = index + 1
  end

  eq(output, items)
  -- eq(output, .value)
end

describe("types", function()
  it("can parse this", function()
    parsed({ "number" }, "number")
  end)

  it("can parse | separated", function()
    parsed({ "number", "bool" }, "number|bool")
  end)

  it("can parse functions", function()
    parsed({ "func: number" }, "func: number", true)
  end)
end)
