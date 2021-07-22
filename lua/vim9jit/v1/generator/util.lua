local trim = vim.trim
local dedent = require("vim9jit.utils").dedent

local get_item

get_item = function(t, param, key, recursive, result_number, current_found)
  if t == nil then
    return nil
  end

  if recursive == nil then
    recursive = false
  end

  if result_number == nil then
    result_number = 1
  end

  if current_found == nil then
    current_found = 0
  end

  if t[param] == key then
    return t
  end

  for k, _ in ipairs(t) do
    if t[k][param] == key then
      current_found = current_found + 1

      if current_found >= result_number then
        return t[k]
      end
    end
  end

  if not recursive then
    return nil
  end

  local result = nil
  for _, v in ipairs(t) do
    if type(v) == "table" then
      result = get_item(v, param, key, result_number, current_found)
      if result then
        current_found = current_found + 1
        if current_found >= result_number then
          return result
        end
      end
    end
  end

  return result
end

local get_item_with_id = function(match, id, recursive)
  return get_item(match, "id", id, recursive)
end

local id_exists = function(match, id)
  return get_item(match, "id", id, false) ~= nil
end

local get_result = function(generator, match)
  if match == nil then
    return nil
  end

  local match_id = match.id
  assert(match_id, string.format("%s:%s malformed object", match_id, match))

  -- Hmm... wonder if I could just use _get_value for a lot of stuff.
  assert(generator.match[match_id], string.format("%s: missing generator", match_id))

  return generator.match[match_id](match)
end

return {
  get_item = get_item,
  get_item_with_id = get_item_with_id,
  id_exists = id_exists,
  get_result = get_result,

  fmt = function(s, ending_newline)
    if string.sub(s, 1, 1) == "\n" then
      s = string.sub(s, 2)
    end

    local eol_string = ""
    if ending_newline == nil or ending_newline == true then
      eol_string = "\n"
    end

    return trim(dedent(s)) .. eol_string
  end,

  get_value = function(match)
    return match.value
  end,
}
