vim.fn.writefile({}, "./data/parsed.txt")
local f = vim.fn.readfile "./data/vim_funcs.txt"

local results = {}

local start = false
local do_line = function(line)
  if not start then
    if line:find "abs" then
      start = true
    end
  end

  if not start then
    return
  end

  print(line)
end

local STATES = {
  PRE = 1,
  NEW = 2,
  MID = 3,
  DEF = 4,
}

local state = STATES.PRE
local definition = ""

local idx = 1
while f[idx] do
  local line = f[idx]

  if state == STATES.PRE then
    if line:find "abs" then
      state = STATES.NEW
    end
  end

  if state == STATES.NEW then
    definition = line
    state = STATES.MID
  elseif state == STATES.MID then
    definition = definition .. line

    if line:find "^#ifdef" then
      error "Oh no, here we go again"
    end

    if line:find "}" then
      state = STATES.NEW
    end

    definition = definition:gsub("\t", " ")
    definition = definition:gsub("%s+", " ")
    definition = vim.trim(definition)
    definition = string.sub(definition, 2, #definition - 2)

    local split = vim.split(definition, ",")
    split = vim.tbl_map(function(v)
      return vim.trim(v)
    end, split)

    split = {
      string.gsub(split[1], '"', ""),
      tonumber(split[2]),
      tonumber(split[3]),
      split[4],
      split[5],
      split[6],
    }

    P(split)

    table.insert(results, split)
  end

  idx = idx + 1
end

vim.fn.writefile(results, "./data/parsed.txt")
vim.cmd [[checktime]]
