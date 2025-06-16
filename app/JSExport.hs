module JSExport ( toJS, T ) where
import Data.List ( intersperse ) -- base
import Text.Printf ( printf ) -- base
import CoreIPG
import GenericExp ( Exp(..) )

-- TODO: Use Text rather than String.
-- TODO: Need to do something about Terminals. If they aren't strings, e.g. Uint8Arrays, then
--       the code below won't work.

type T = String
type Expr = Exp T

refToJS :: Ref T T Expr -> T
refToJS (Id f) = printf "self.%s" f
refToJS (Attr nt f) = printf "nt_%s.%s" nt f
refToJS (Index nt e f) = printf "seq_%s[%s].%s" nt (exprToJS e) f
refToJS EOI = "EOI";
refToJS (Start nt) = printf "nt_%s._ipg_start" nt;
refToJS (End nt) = printf "nt_%s._ipg_end" nt;

exprToJS :: Expr -> T
exprToJS e = exprToJS' 0 e ""

-- %right '=' '?' -- 2
-- %left '||' -- 3
-- %left '&&' -- 4
-- %left '|' -- 5
-- %left '^' -- 6
-- %left '&' -- 7
-- %left '==' '!=' -- 8
-- %left '<' '>' '<=' '>=' -- 9
-- %left '<<' '>>' -- 10
-- %left '+' '-' -- 11
-- %left '*' '/' '%' -- 12
-- %right '**' -- 13
-- %nonassoc NEG PLUS '~' '!' -- 14
-- %nonassoc '[' -- 17
-- %left '.' -- 17

exprToJS' :: Int -> Expr -> T -> T
exprToJS' _ (Int n) = shows n
exprToJS' _ (Float n) = shows n
exprToJS' _ (String s) = shows s
exprToJS' p (Add l r) = showParen (p > 11) (exprToJS' 11 l . (" + "++) . exprToJS' 12 r)
exprToJS' p (Sub l r) = showParen (p > 11) (exprToJS' 11 l . (" - "++) . exprToJS' 12 r)
exprToJS' p (Mul l r) = showParen (p > 12) (exprToJS' 12 l . (" * "++) . exprToJS' 13 r)
exprToJS' p (Div l r) = showParen (p > 12) (exprToJS' 12 l . (" / "++) . exprToJS' 13 r)
exprToJS' p (Mod l r) = showParen (p > 12) (exprToJS' 12 l . (" % "++) . exprToJS' 13 r)
exprToJS' p (Exp l r) = showParen (p > 13) (exprToJS' 14 l . (" ** "++) . exprToJS' 13 r)
exprToJS' p (Neg e) = showParen (p > 14) (('-':) . exprToJS' 15 e)
exprToJS' p (BitwiseNeg e) = showParen (p > 14) (('~':) . exprToJS' 15 e)
exprToJS' p (And l r) = showParen (p > 4) (exprToJS' 4 l . (" && "++) . exprToJS' 5 r)
exprToJS' p (Or l r) = showParen (p > 3)  (exprToJS' 3 l . (" || "++) . exprToJS' 4 r)
exprToJS' p (BitwiseAnd l r) = showParen (p > 7) (exprToJS' 7 l . (" & "++) . exprToJS' 8 r)
exprToJS' p (BitwiseXor l r) = showParen (p > 6)  (exprToJS' 6 l . (" ^ "++) . exprToJS' 7 r)
exprToJS' p (BitwiseOr l r) = showParen (p > 5)  (exprToJS' 5 l . (" | "++) . exprToJS' 6 r)
exprToJS' p (LSh l r) = showParen (p > 10) (exprToJS' 10 l . (" << "++) . exprToJS' 11 r)
exprToJS' p (RSh l r) = showParen (p > 10) (exprToJS' 10 l . (" >> "++) . exprToJS' 11 r)
exprToJS' p (LessThan l r) = showParen (p > 9) (exprToJS' 9 l . (" < "++) . exprToJS' 10 r)
exprToJS' p (LTE l r) = showParen (p > 9) (exprToJS' 9 l . (" <= "++) . exprToJS' 10 r)
exprToJS' p (GreaterThan l r) = showParen (p > 9) (exprToJS' 9 l . (" > "++) . exprToJS' 10 r)
exprToJS' p (GTE l r) = showParen (p > 9) (exprToJS' 9 l . (" >= "++) . exprToJS' 10 r)
exprToJS' p (Equal l r) = showParen (p > 8) (exprToJS' 8 l . (" == "++) . exprToJS' 9 r)
exprToJS' p (NotEqual l r) = showParen (p > 8) (exprToJS' 8 l . (" != "++) . exprToJS' 9 r)
exprToJS' p (Not l) = showParen (p > 14) (('!':) . exprToJS' 15 l)
exprToJS' p (If b t e) =
    showParen (p > 2) (exprToJS' 2 b . (" ? "++) . exprToJS' 3 t . (" : "++) . exprToJS' 3 e)
exprToJS' _ (Call t es) =
    shows t . ('(':) . foldr (.) id (intersperse (',':) $ map (exprToJS' 0) es) . (')':)
exprToJS' p (At l r) = showParen (p > 17) (exprToJS' 17 l . ('[':) . exprToJS' 0 r . (']':))
exprToJS' _ (Ref r) = (refToJS r++)

termToJS :: Term T T T Expr -> T
termToJS (NonTerminal nt l r)
    = printf "    // %s[%s, %s]\n" nt lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "    nt_%s = %s(input.slice(left, right));\n" nt nt
   <> printf "    if (nt_%s === null) break _ipg_alt;\n" nt
   <> printf "    if (nt_%s._ipg_end !== 0) {\n" nt
   <> printf "      self._ipg_start = Math.min(self._ipg_start, left + nt_%s._ipg_start);\n" nt
   <> printf "      self._ipg_end = Math.max(self._ipg_end, left + nt_%s._ipg_end);\n" nt
   <> printf "      nt_%s._ipg_end += left;\n" nt
   <> printf "      nt_%s._ipg_start += left;\n" nt
   <>        "    }\n\n"
  where lExp = exprToJS l; rExp = exprToJS r
termToJS (Terminal "" l r) 
    = printf "    // ""[%s, %s]\n" lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n\n"
  where lExp = exprToJS l; rExp = exprToJS r
termToJS (Terminal t l r)
    = printf "    // %s[%s, %s]\n" (show t) lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "    if (!input.slice(left, right).startsWith(%s)) break _ipg_alt;\n" (show t)
   <>        "    self._ipg_start = Math.min(self._ipg_start, left);\n"
   <>        "    self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS l; rExp = exprToJS r
termToJS (i := e)
    = printf "    // {%s = %s}\n" i eExp
   <> printf "    self.%s = %s;\n\n" i eExp
  where eExp = exprToJS e
termToJS (Guard e)
    = printf "    // ?[%s]\n" eExp
   <> printf "    if (!%s) break _ipg_alt;\n\n" eExp
  where eExp = exprToJS' 15 e ""
termToJS (Array i start end nt l r)
    = printf "    // for %s = %s to %s do %s[%s, %s]\n" i startExp endExp nt lExp rExp
   <> printf "    seq_%s = [];\n" nt
   <> printf "    for (self.%s = %s; self.%s < %s; self.%s++) {\n"
            i startExp i endExp i
   <> printf "      const left = %s;\n" lExp
   <> printf "      const right = %s;\n" rExp
   <>        "      if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "      const tmp = %s(input.slice(left, right));\n" nt
   <>        "      if (tmp === null) break _ipg_alt;\n"
   <>        "      if (tmp._ipg_end !== 0) {\n"
   <>        "        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);\n"
   <>        "        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);\n"
   <>        "        tmp._ipg_end += left;\n"
   <>        "        tmp._ipg_start += left;\n"
   <>        "       }\n"
   <> printf "      seq_%s.push(tmp);\n" nt
   <>        "    }\n"
   <> printf "    delete self.%s;\n\n" i
  where startExp = exprToJS start; endExp = exprToJS' 10 end "";
        lExp = exprToJS l; rExp = exprToJS r
termToJS (Any i l)
    = printf "    // {%s = .[%s]}\n" i lExp
   <> printf "    left = %s;\n" lExp
   <>        "    right = left + 1;\n"
   <>        "    if (left < 0 || right > EOI) break _ipg_alt;\n"
   <> printf "    self.%s = input[left];\n" i
   <>        "    self._ipg_start = Math.min(self._ipg_start, left);\n"
   <>        "    self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS l
termToJS (Slice i l r)
    = printf "    // {%s = *[%s, %s]}\n" i lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "    self.%s = input.slice(left, right);\n" i
   <>        "    if (left !== right) {\n"
   <>        "      self._ipg_start = Math.min(self._ipg_start, left);\n"
   <>        "      self._ipg_end = Math.max(self._ipg_end, right);\n"
   <>        "    }\n\n"
  where lExp = exprToJS l; rExp = exprToJS r

alternativeToJS :: Alternative T T T Expr -> T
alternativeToJS (Alternative ts)
    = "  _ipg_alt: {\n"
   <> "    let left; let right;\n"
   <>      concatMap declare nts
   <> "    self = { _ipg_start: EOI, _ipg_end: 0 };\n\n"
   <>      concatMap termToJS ts
   <> "    return self;\n"
   <> "  }\n"
  where nts = nonArrayNonTerminals ts
        declare nt = printf "    let nt_%s;\n" nt
    
ruleToJS :: Rule T T T Expr -> T
ruleToJS (Rule nt alts)
    = printf "function %s(input) {\n" nt
          <> "  const EOI = input.length;\n"
          <> "  let self = { _ipg_start: EOI, _ipg_end: 0 };\n"
          <> concatMap alternativeToJS alts
          <> "  return null;\n}\n\n"

toJS :: Grammar T T T Expr -> T
toJS (Grammar rules) = concatMap ruleToJS rules
