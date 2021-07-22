local L = require "lpeg"

local grammar = {}

grammar.track_positions = false

local _match_state = {}
local function getline(s, p)
  if _match_state.line_index[s] == nil then
    local idx = { [0] = 1 }
    local inl = 0
    while true do
      inl = string.find(s, "\n", inl + 1, true)
      if inl then
        table.insert(idx, inl)
      else
        break
      end
    end
    if idx[#idx] ~= #s then
      table.insert(idx, #s)
    end
    _match_state.line_index[s] = idx
  end

  local idx = _match_state.line_index[s]
  assert(#idx > 0)
  assert(idx[#idx] >= p)

  local min = 1
  local max = #idx
  local lno
  while true do
    lno = math.floor((min + max) / 2)
    local q = idx[lno]
    if p == q then
      break
    elseif p > q then
      min = lno + 1
      if min > max then
        lno = min
        break
      end
    else
      max = lno - 1
      if min > max then
        break
      end
    end
  end
  return lno, idx[lno - 1]
end

local function make_ast_node(id, pos, t)
  t.id = id

  -- TODO: Can add back pos if we need to
  -- t.pos = pos

  -- Place a value
  if t[1] and type(t[1]) == "string" then
    t.value = t[1]
    -- I don't want to see any strings that are just sitting there
    table.remove(t, 1)
  end

  -- Get the start and finish positions
  local pos_end = pos
  if t.value then
    pos_end = pos + #t.value - 1
  end

  if grammar.track_positions then
    local lno, sol = getline(_match_state.current_string, pos)
    local lno_end, sol_end = getline(_match_state.current_string, pos_end)
    -- TODO: seems bad
    if (pos == 1) and (sol == 1) then
      sol = 0
    end

    t.pos = {
      line_start = lno,
      line_finish = lno_end,
      char_start = pos - sol,
      char_finish = pos_end - sol_end,
      byte_start = pos,
      byte_finish = pos_end,
    }
  end

  return t
end

grammar.grammar = function(t)
  assert(type(t) == "table", "t must be a table")

  local result = {}
  for k, v in pairs(t) do
    if k == 1 then
      result[k] = v
    else
      result[k] = (
                    -- string of name
L.Cc(k)
          -- position captured
          * L.Cp()
          -- table containing match data
          * L.Ct(v)
        ) / make_ast_node
    end
  end

  return L.P(result)
end

grammar.match = function(g, str)
  _match_state = {}
  _match_state.line_index = {}
  _match_state.current_string = str

  return L.match(g, str, 1, "grammar")
end

return grammar
