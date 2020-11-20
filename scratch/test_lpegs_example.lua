local lpeg = require('lpeg')

-- Lexical Elements
local Space = lpeg.S(" \n\t")^0
local Number = lpeg.C(lpeg.P"-"^-1 * lpeg.R("09")^1) * Space
local TermOp = lpeg.C(lpeg.S("+-")) * Space
local FactorOp = lpeg.C(lpeg.S("*/")) * Space
local Open = "(" * Space
local Close = ")" * Space

-- Grammar
local Exp, Term, Factor = lpeg.V"Exp", lpeg.V"Term", lpeg.V"Factor"
G = lpeg.P{ Exp,
  Exp = lpeg.Ct(Term * (TermOp * Term)^0);
  Term = lpeg.Ct(Factor * (FactorOp * Factor)^0);
  Factor = Number + Open * Exp * Close;
}

G = Space * G * -1

-- Evaluator
function eval (x)
  if type(x) == "string" then
    return tonumber(x)
  else
    local op1 = eval(x[1])
    for i = 2, #x, 2 do
      local op = x[i]
      local op2 = eval(x[i + 1])
      if (op == "+") then op1 = op1 + op2
      elseif (op == "-") then op1 = op1 - op2
      elseif (op == "*") then op1 = op1 * op2
      elseif (op == "/") then op1 = op1 / op2
      end
    end
    return op1
  end
end

-- Parser/Evaluator
function evalExp (s)
  local t = lpeg.match(G, s)
  print(vim.inspect(t))
  if not t then error("syntax error", 2) end
  return eval(t)
end

-- small example
print(evalExp"1 + 1 + 1 + 1 * (2 + 7)")   --> 13.5
