local g = require "vim9jit.grammar"
local lib = require "vim9jit.lib"

local any_whitespace = lib.any_whitespace
local one_or_no = lib.one_or_no
local letter = lib.letter
local list_of = lib.list_of
local one_or_more = lib.one_or_more
local seq = lib.seq
local set = lib.set

local group = setmetatable({}, {
  __index = function(_, k)
    return lib.group(k)
  end,
})

return g.grammar {
  "TypeSyntax",

  TypeDefinition = seq {
    ":",
    group.TypeSyntax,
  },

  TypeSyntax = list_of {
    set {
      group.TypeFunctionDefinition,
      group.TypeSingleDefinition,
    },

    separator = "|",
  },

  TypeSingleDefinition = one_or_more { letter, capture = true },

  TypeFunctionDefinition = seq {
    "func",
    set {
      ":",
      seq {
        "(",
        set {
          seq {
            list_of { group.TypeSyntax, separator = "," },
            one_or_no { seq { ",", "...", linespace = true } },
          },
          "...",
        },
        ")",
        ":",

        linespace = true,
      },
    },
    group.TypeSyntax,

    linespace = true,
  },
}
