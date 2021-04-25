
-- I have changed several things in this file, but the majority of the ideas and compat between versions
-- is thanks to: github.com epnfs.lua

local L = require('lpeg')

-- sanity check? {{{
assert(error)
assert(io)
assert(string)
assert(next)
assert(pairs)
assert(setmetatable)
assert(tostring)
assert(type)
-- }}}

-- luacheck: ignore
-- Get a nice print for vim, if applicable
local nvim = (vim or {})
local nvim = (nvim.api or {})
local print = nvim.nvim_err_write or print

local V = string.sub(assert(_VERSION), -4)

if V == " 5.1" then
  assert(setfenv)
  assert(getfenv)
end


-- module table
local epnf = {}
epnf.throw_error = false
epnf.print_error = true
-- TODO: Put this in my print statements
epnf.suppress_messages = false

epnf.max_width = 120


-- maximum of two numbers while avoiding math lib as a dependency
local function max( a, b )
  if a < b then return b else return a end
end


-- get the the line number and start for p
local __line_index = {}
local function getline( s, p )
  if (__line_index[s] == nil) then
    local idx = {[0]=1}
    local inl = 0
    while true do
      inl = string.find(s, "\n", inl+1, true)
      if inl then
        table.insert(idx, inl)
      else
        break
      end
    end
    if idx[#idx] ~= #s then
      table.insert(idx, #s)
    end
    __line_index[s] = idx
  end

  local idx = __line_index[s]
  assert(#idx > 0)
  assert(idx[#idx] >= p)

  local min = 1
  local max = #idx
  local lno
  while true do
    lno = math.floor((min+max)/2)
    local q = idx[lno]
    if p == q then
      break
    elseif p > q then
      min = lno+1
      if min > max then
        lno = min
        break
      end
    else
      max = lno-1
      if min > max then
        break
      end
    end
  end
  return lno, idx[lno-1]
end

-- Error reporting {{{
-- raise an error during semantic validation of the ast
local function raise_error( n, msg, s, p )
  local lno, sol = getline( s, p )
  local eol = string.find(s, "\n", sol+1, true) or (#s + 1)
  local line = string.sub(s, sol, eol-1)
  assert( p <= #s )
  local clen = max( epnf.max_width, p+10-sol )
  if #line > clen then
    line = string.sub( line, 1, clen ) .. "..."
  end
  local marker = string.rep( " ", p-sol ) .. "^"
  local final_msg = n..":"..lno..": "..msg.."\n"..line.."\n"..marker.."\n"

  if epnf.throw_error then
    error(final_msg,  0)
  elseif epnf.print_error then
    print(final_msg)
  end
end

-- parse-error reporting function
local function parse_error(s, p, n, e)
  if p <= #s then
    local msg = "parse error"
    if e then msg = msg .. ", " .. e end
    raise_error( n, msg, s, p )
  else -- parse error at end of input
    local _,lno = string.gsub( s, "\n", "\n" )
    if string.sub( s, -1, -1 ) ~= "\n" then lno = lno + 1 end
    local msg = ": parse error at <eof>"
    if e then msg = msg .. ", " .. e end

    if epnf.throw_error then
      error( n..":"..lno..msg, 0 )
    elseif epnf.print_error then
      print( n .. ":" .. lno .. msg)
    end
  end
end
-- }}}

local function make_ast_node( id, pos, t )
  t.id = id

  local lno, sol = getline( epnf.current_string, pos )

  -- Place a value
  if t[1] and type(t[1]) == 'string' then
    t.value = t[1]
    -- I don't want to see any strings that are just sitting there
    table.remove(t, 1)
  end

  -- Get the start and finish positions
  local pos_end = pos
  if t.value then
    pos_end = pos + #t.value - 1
  end

  local lno_end, sol_end = getline(epnf.current_string, pos_end)
  -- TODO: seems bad
  if (pos == 1) and (sol == 1) then
    sol = 0
  end

  t.pos = {
    line_start  = lno,
    line_finish = lno_end,
    char_start  = pos - sol,
    char_finish = pos_end - sol_end,
    byte_start  = pos,
    byte_finish = pos_end,
  }

  return t
end


-- some useful/common lpeg patterns
local L_Carg_1 = L.Carg( 1 )
local function E( msg )
  return L.Cmt( L_Carg_1 * L.Cc( msg ), parse_error )
end


-- setup an environment where you can easily define lpeg grammars
-- with lots of syntax sugar
function epnf.define( func, g, e )
  g = g or {}
  if e == nil then
    e = V == " 5.1" and getfenv( func ) or _G
  end
  local suppressed = {}
  local env = {}
  local env_index = {
    START = function( name ) g[ 1 ] = name end,
    SUPPRESS = function( ... )
      suppressed = {}
      for i = 1, select( '#', ... ) do
        suppressed[ select( i, ... ) ] = true
      end
    end,

    E = E,
  }

  setmetatable( env_index, { __index = e } )
  setmetatable( env, {
    __index = env_index,
    __newindex = function( _, name, val )
      if suppressed[ name ] then
        g[ name ] = val
      else
        g[ name ] = (
            -- string of name
            L.Cc(name)
            -- position captured
            * L.Cp()
            -- table containing match data
            * L.Ct(val)
          ) / make_ast_node
      end
    end
  } )
  -- call passed function with custom environment (5.1- and 5.2-style)
  if V == " 5.1" then
    setfenv( func, env )
  end
  func( env )
  assert( g[ 1 ] and g[ g[ 1 ] ], "no start rule defined" )
  return g
end


-- apply a given grammar to a string and return the ast. also allows
-- to set the name of the string for error messages
function epnf.parse( g, name, input, ... )
  __line_index = {}
  return L.match( L.P( g ), input, 1, name, ... ), name, input
end


-- apply a given grammar to the contents of a file and return the ast
function epnf.parsefile( g, fname, ... )
  local f = assert( io.open( fname, "r" ) )
  local a,n,i = epnf.parse( g, fname, assert( f:read"*a" ), ... )
  f:close()
  return a,n,i
end


-- apply a given grammar to a string and return the ast. automatically
-- picks a sensible name for error messages
function epnf.parsestring( g, str, ... )
  local s = string.sub( str, 1, 20 )
  epnf.current_string = str
  if #s < #str then s = s .. "..." end
  local name = "[\"" .. string.gsub( s, "\n", "\\n" ) .. "\"]"
  return epnf.parse( g, name, str, ... )
end

-- parse a string, but its known that it is an incremental section of the string
-- so you must also specify a starting line
function epnf.parse_incremental(g, str, transform, ...)
  local s = string.sub(str, 1, 20)
  epnf.current_string = str
  if #s < #str then s = s .. "..." end
  local name = "[\"" .. string.gsub( s, "\n", "\\n" ) .. "\"]"
  local parsed = epnf.parse( g, name, str, ... )

  return transform(parsed, ...)
end


local function write( ... ) return io.stderr:write( ... ) end
local function dump_ast( node, prefix )
  if type( node ) == "table" then
    write( "{" )
    if next( node ) ~= nil then
      write( "\n" )
      if type( node.id ) == "string" and
         type( node.pos ) == "number" then
        write( prefix, "  id = ", node.id,
               ",  pos = ", tostring( node.pos ), "\n" )
      end
      for k,v in pairs( node ) do
        if k ~= "id" and k ~= "pos" then
          write( prefix, "  ", tostring( k ), " = " )
          dump_ast( v, prefix.."  " )
        end
      end
    end
    write( prefix, "}\n" )
  else
    write( tostring( node ), "\n" )
  end
end

-- write a string representation of the given ast to stderr for
-- debugging
function epnf.dumpast( node )
  return dump_ast( node, "" )
end


-- export a function for reporting errors during ast validation
epnf.raise = raise_error


-- return module table
return epnf
