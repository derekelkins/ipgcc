module JSExport ( toJS, T ) where
import qualified Data.Set as Set -- containers
import Data.List ( intersperse ) -- base
import Text.Printf ( printf ) -- base
import CoreIPG
import GenericExp ( Exp(..) )

-- TODO: Use Text rather than String.
-- TODO: Need to do something about Terminals. If they aren't strings, e.g. Uint8Arrays, then
--       the code below won't work.

type T = String
type Expr = Exp T
type Env = Set.Set T

refToJS :: Env -> Ref T T Expr -> T
refToJS env (Id f) | f `Set.member` env = printf "a_%s" f
                   | otherwise          = printf "self.%s" f
refToJS _   (Attr nt f) = printf "nt_%s.%s" nt f
refToJS env (Index nt e f) = printf "seq_%s[%s].%s" nt (exprToJS env e) f
refToJS _   EOI = "EOI";
refToJS _   (Start nt) = printf "nt_%s._ipg_start" nt;
refToJS _   (End nt) = printf "nt_%s._ipg_end" nt;

exprToJS :: Env -> Expr -> T
exprToJS env e = exprToJS' env 0 e ""

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

exprToJS' :: Env -> Int -> Expr -> T -> T
exprToJS' _ _ (Int n) = shows n
exprToJS' _ _ (Float n) = shows n
exprToJS' _ _ (String s) = shows s
exprToJS' env p (Add l r) =
    showParen (p > 11) (exprToJS' env 11 l . (" + "++) . exprToJS' env 12 r)
exprToJS' env p (Sub l r) =
    showParen (p > 11) (exprToJS' env 11 l . (" - "++) . exprToJS' env 12 r)
exprToJS' env p (Mul l r) =
    showParen (p > 12) (exprToJS' env 12 l . (" * "++) . exprToJS' env 13 r)
exprToJS' env p (Div l r) =
    showParen (p > 12) (exprToJS' env 12 l . (" / "++) . exprToJS' env 13 r)
exprToJS' env p (Mod l r) =
    showParen (p > 12) (exprToJS' env 12 l . (" % "++) . exprToJS' env 13 r)
exprToJS' env p (Exp l r) =
    showParen (p > 13) (exprToJS' env 14 l . (" ** "++) . exprToJS' env 13 r)
exprToJS' env p (Neg e) =
    showParen (p > 14) (('-':) . exprToJS' env 15 e)
exprToJS' env p (BitwiseNeg e) =
    showParen (p > 14) (('~':) . exprToJS' env 15 e)
exprToJS' env p (And l r) =
    showParen (p > 4) (exprToJS' env 4 l . (" && "++) . exprToJS' env 5 r)
exprToJS' env p (Or l r) =
    showParen (p > 3)  (exprToJS' env 3 l . (" || "++) . exprToJS' env 4 r)
exprToJS' env p (BitwiseAnd l r) =
    showParen (p > 7) (exprToJS' env 7 l . (" & "++) . exprToJS' env 8 r)
exprToJS' env p (BitwiseXor l r) =
    showParen (p > 6)  (exprToJS' env 6 l . (" ^ "++) . exprToJS' env 7 r)
exprToJS' env p (BitwiseOr l r) =
    showParen (p > 5)  (exprToJS' env 5 l . (" | "++) . exprToJS' env 6 r)
exprToJS' env p (LSh l r) =
    showParen (p > 10) (exprToJS' env 10 l . (" << "++) . exprToJS' env 11 r)
exprToJS' env p (RSh l r) =
    showParen (p > 10) (exprToJS' env 10 l . (" >> "++) . exprToJS' env 11 r)
exprToJS' env p (LessThan l r) =
    showParen (p > 9) (exprToJS' env 9 l . (" < "++) . exprToJS' env 10 r)
exprToJS' env p (LTE l r) =
    showParen (p > 9) (exprToJS' env 9 l . (" <= "++) . exprToJS' env 10 r)
exprToJS' env p (GreaterThan l r) =
    showParen (p > 9) (exprToJS' env 9 l . (" > "++) . exprToJS' env 10 r)
exprToJS' env p (GTE l r) =
    showParen (p > 9) (exprToJS' env 9 l . (" >= "++) . exprToJS' env 10 r)
exprToJS' env p (Equal l r) =
    showParen (p > 8) (exprToJS' env 8 l . (" == "++) . exprToJS' env 9 r)
exprToJS' env p (NotEqual l r) =
    showParen (p > 8) (exprToJS' env 8 l . (" != "++) . exprToJS' env 9 r)
exprToJS' env p (Not l) =
    showParen (p > 14) (('!':) . exprToJS' env 15 l)
exprToJS' env p (If b t e) =
    showParen (p > 2)
        (exprToJS' env 2 b . (" ? "++) . exprToJS' env 3 t . (" : "++) . exprToJS' env 3 e)
exprToJS' env _ (Call t es) =
    shows t . ('(':) . foldr (.) id (intersperse (',':) $ map (exprToJS' env 0) es) . (')':)
exprToJS' env p (At l r) =
    showParen (p > 17) (exprToJS' env 17 l . ('[':) . exprToJS' env 0 r . (']':))
exprToJS' env _ (Ref r) = (refToJS env r++)

paramList :: [T] -> T
paramList = concatMap (", a_"++)

argList :: Env -> [Expr] -> T
argList env = concatMap ((',':) . (' ':) . exprToJS env)

termToJS :: Env -> Term T T T Expr -> T
termToJS env (NonTerminal nt args l r)
    = printf "    // %s{%s}[%s, %s]\n" nt (drop 2 es) lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "    nt_%s = %s(input.slice(left, right)%s);\n" nt nt es
   <> printf "    if (nt_%s === null) break _ipg_alt;\n" nt
   <> printf "    if (nt_%s._ipg_end !== 0) {\n" nt
   <> printf "      self._ipg_start = Math.min(self._ipg_start, left + nt_%s._ipg_start);\n" nt
   <> printf "      self._ipg_end = Math.max(self._ipg_end, left + nt_%s._ipg_end);\n" nt
   <> printf "      nt_%s._ipg_end += left;\n" nt
   <> printf "      nt_%s._ipg_start += left;\n" nt
   <>        "    }\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r; es = argList env args
termToJS env (Terminal "" l r) 
    = printf "    // ""[%s, %s]\n" lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r
termToJS env (Terminal t l r)
    = printf "    // %s[%s, %s]\n" (show t) lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "    if (!input.slice(left, right).startsWith(%s)) break _ipg_alt;\n" (show t)
   <>        "    self._ipg_start = Math.min(self._ipg_start, left);\n"
   <>        "    self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r
termToJS env (i := e)
    = printf "    // {%s = %s}\n" i eExp
   <> printf "    self.%s = %s;\n\n" i eExp
  where eExp = exprToJS env e
termToJS env (Guard e)
    = printf "    // ?[%s]\n" eExp
   <> printf "    if (!%s) break _ipg_alt;\n\n" eExp
  where eExp = exprToJS' env 15 e ""
termToJS env (Array i start end nt args l r)
    = printf "    // for %s = %s to %s do %s{%s}[%s, %s]\n"
        i startExp endExp nt (drop 2 es) lExp rExp
   <> printf "    seq_%s = [];\n" nt
   <> printf "    for (self.%s = %s; self.%s < %s; self.%s++) {\n"
            i startExp i endExp i
   <> printf "      const left = %s;\n" lExp
   <> printf "      const right = %s;\n" rExp
   <>        "      if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "      const tmp = %s(input.slice(left, right)%s);\n" nt es
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
  where startExp = exprToJS env start; endExp = exprToJS' env 10 end "";
        lExp = exprToJS env l; rExp = exprToJS env r; es = argList env args
termToJS env (Any i l)
    = printf "    // {%s = .[%s]}\n" i lExp
   <> printf "    left = %s;\n" lExp
   <>        "    right = left + 1;\n"
   <>        "    if (left < 0 || right > EOI) break _ipg_alt;\n"
   <> printf "    self.%s = input[left];\n" i
   <>        "    self._ipg_start = Math.min(self._ipg_start, left);\n"
   <>        "    self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS env l
termToJS env (Slice i l r)
    = printf "    // {%s = *[%s, %s]}\n" i lExp rExp
   <> printf "    left = %s;\n" lExp
   <> printf "    right = %s;\n" rExp
   <>        "    if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> printf "    self.%s = input.slice(left, right);\n" i
   <>        "    if (left !== right) {\n"
   <>        "      self._ipg_start = Math.min(self._ipg_start, left);\n"
   <>        "      self._ipg_end = Math.max(self._ipg_end, right);\n"
   <>        "    }\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r

alternativeToJS :: Env -> Alternative T T T Expr -> T
alternativeToJS env (Alternative ts)
    = "  _ipg_alt: {\n"
   <> "    let left; let right;\n"
   <>      concatMap declare nts
   <> "    self = { _ipg_start: EOI, _ipg_end: 0 };\n\n"
   <>      concatMap (termToJS env) ts
   <> "    return self;\n"
   <> "  }\n"
  where nts = nonArrayNonTerminals ts
        declare nt = printf "    let nt_%s;\n" nt
    
ruleToJS :: Rule T T T Expr -> T
ruleToJS (Rule nt args alts)
    = printf "function %s(input%s) {\n" nt (paramList args)
          <> "  const EOI = input.length;\n"
          <> "  let self = { _ipg_start: EOI, _ipg_end: 0 };\n"
          <> concatMap (alternativeToJS env) alts
          <> "  return null;\n}\n\n"
  where env = Set.fromList args

toJS :: Grammar T T T Expr -> T
toJS (Grammar rules) = concatMap ruleToJS rules
