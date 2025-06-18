module JSExport ( toJS, T ) where
import Data.Char ( isPrint, ord ) -- base
import qualified Data.Set as Set -- containers
import Numeric ( showHex ) -- base
import Data.List ( intersperse ) -- base
import Text.Printf ( printf ) -- base
import CoreIPG
import GenericExp ( Exp(..) )

-- TODO: Use Text rather than String.

type T = String
type Expr = Exp T
type Env = Set.Set T

refToJS :: Env -> Ref T T Expr -> T
refToJS env (Id f) | f `Set.member` env = printf "a_%s" f
                   | otherwise          = printf "_ipg_lookup(self, '%s')" f
refToJS _   (Attr nt "this") = printf "(({_ipg_start,_ipg_end,_ipg_parent,...o}) => o)(nt_%s)" nt
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
    (t++) . ('(':) . foldr (.) id (intersperse (", "++) $ map (exprToJS' env 0) es) . (')':)
exprToJS' env p (At l r) =
    showParen (p > 17) (exprToJS' env 17 l . ('[':) . exprToJS' env 0 r . (']':))
exprToJS' env _ (Ref r) = (refToJS env r++)

paramList :: [T] -> T
paramList = concatMap (", a_"++)

argList :: Env -> [Expr] -> T
argList env = concatMap ((',':) . (' ':) . exprToJS env)

call :: T -> T
call "" = ""
call es = '(':drop 2 es ++ ")"

hexyString :: String -> String
hexyString s = '"':concatMap go s ++ "\""
    where go c | isPrint c = [c]
               | c `elem` "abfnrtv\\\"'" = show c
               | otherwise = '\\':'x':pad (showHex (ord c) "")
          pad h@[_] = '0':h
          pad h = h

termToJS :: T -> Env -> Term T T T Expr -> T
termToJS indent env (NonTerminal nt args l r)
    = indent <> printf "// %s%s[%s, %s]\n" nt (call es) lExp rExp
   <> indent <> printf "left = %s;\n" lExp
   <> indent <> printf "right = %s;\n" rExp
   <> indent <>        "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> printf "nt_%s = %s(input.slice(left, right)%s);\n" nt nt es
   <> indent <> printf "if (nt_%s === null) break _ipg_alt;\n" nt
   <> indent <> printf "if (nt_%s._ipg_end !== 0) {\n" nt
   <> indent <> printf "  self._ipg_start = Math.min(self._ipg_start, left + nt_%s._ipg_start);\n" nt
   <> indent <> printf "  self._ipg_end = Math.max(self._ipg_end, left + nt_%s._ipg_end);\n" nt
   <> indent <> printf "  nt_%s._ipg_end += left;\n" nt
   <> indent <> printf "  nt_%s._ipg_start += left;\n" nt
   <> indent <>        "}\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r; es = argList env args
termToJS indent env (Terminal "" l r) 
    = indent <> printf "// ""[%s, %s]\n" lExp rExp
   <> indent <> printf "left = %s;\n" lExp
   <> indent <> printf "right = %s;\n" rExp
   <> indent <>        "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r
termToJS indent env (Terminal t l r)
    = indent <> printf "// %s[%s, %s]\n" terminal lExp rExp
   <> indent <> printf "left = %s;\n" lExp
   <> indent <> printf "right = %s;\n" rExp
   <> indent <>        "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> printf "if (!_ipg_startsWith(input.slice(left, right), %s)) break _ipg_alt;\n" terminal
   <> indent <>        "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>        "self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r; terminal = hexyString t
termToJS indent env (i := e)
    = indent <> printf "// {%s = %s}\n" i eExp
   <> indent <> printf "self.%s = %s;\n\n" i eExp
  where eExp = exprToJS env e
termToJS indent env (Guard e)
    = indent <> printf "// ?[%s]\n" eExp
   <> indent <> printf "if (!%s) break _ipg_alt;\n\n" eExp
  where eExp = exprToJS' env 15 e ""
termToJS indent env (Array i start end nt args l r)
    = indent <> printf "// for %s = %s to %s do %s%s[%s, %s]\n"
        i startExp endExp nt (call es) lExp rExp
   <> indent <> printf "seq_%s = [];\n" nt
   <> indent <> printf "for (self.%s = %s; self.%s < %s; self.%s++) {\n"
            i startExp i endExp i
   <> indent <> printf "  const left = %s;\n" lExp
   <> indent <> printf "  const right = %s;\n" rExp
   <> indent <>        "  if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> printf "  const tmp = %s(input.slice(left, right)%s);\n" nt es
   <> indent <>        "  if (tmp === null) break _ipg_alt;\n"
   <> indent <>        "  if (tmp._ipg_end !== 0) {\n"
   <> indent <>        "    self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);\n"
   <> indent <>        "    self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);\n"
   <> indent <>        "    tmp._ipg_end += left;\n"
   <> indent <>        "    tmp._ipg_start += left;\n"
   <> indent <>        "   }\n"
   <> indent <> printf "  seq_%s.push(tmp);\n" nt
   <> indent <>        "}\n"
   <> indent <> printf "delete self.%s;\n\n" i
  where startExp = exprToJS env start; endExp = exprToJS' env 10 end "";
        lExp = exprToJS env l; rExp = exprToJS env r; es = argList env args
termToJS indent env (Any i l)
    = indent <> printf "// {%s = .[%s]}\n" i lExp
   <> indent <> printf "left = %s;\n" lExp
   <> indent <>        "right = left + 1;\n"
   <> indent <>        "if (left < 0 || right > EOI) break _ipg_alt;\n"
   <> indent <> printf "self.%s = input[left];\n" i
   <> indent <>        "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>        "self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS env l
termToJS indent env (Slice i l r)
    = indent <> printf "// {%s = *[%s, %s]}\n" i lExp rExp
   <> indent <> printf "left = %s;\n" lExp
   <> indent <> printf "right = %s;\n" rExp
   <> indent <>        "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> printf "self.%s = input.slice(left, right);\n" i
   <> indent <>        "if (left !== right) {\n"
   <> indent <>        "  self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>        "  self._ipg_end = Math.max(self._ipg_end, right);\n"
   <> indent <>        "}\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r
termToJS indent env (Repeat nt1 args1 i nt2 args2)
    = indent <> printf "// repeat %s%s.%s until %s%s\n" nt1 (call es1) i nt2 (call es2)
   <> indent <>        "right = EOI;\n"
   <> indent <>        "self.values = [];\n"
   <> indent <>        "while(true) {\n"
   <> indent <>        "  left = self._ipg_end;\n" 
   <> indent <>        "  if (right < left) break _ipg_alt;\n"
   <> indent <> printf "  nt_%s = %s(input.slice(left, right)%s);\n" nt2 nt2 es2
   <> indent <> printf "  if (nt_%s !== null) {\n" nt2
   <> indent <> printf "    if (nt_%s._ipg_end !== 0) {\n" nt2
   <> indent <> printf "      self._ipg_start = Math.min(self._ipg_start, left + nt_%s._ipg_start);\n" nt2
   <> indent <> printf "      self._ipg_end = Math.max(self._ipg_end, left + nt_%s._ipg_end);\n" nt2
   <> indent <> printf "      nt_%s._ipg_end += left;\n" nt2
   <> indent <> printf "      nt_%s._ipg_start += left;\n" nt2
   <> indent <>        "    }\n"
   <> indent <>        "    break;\n"
   <> indent <>        "  }\n"
   <> indent <>        "  if (right < left) break _ipg_alt;\n"
   <> indent <> printf "  nt_%s = %s(input.slice(left, right)%s);\n" nt1 nt1 es1
   <> indent <> printf "  if (nt_%s === null) break _ipg_alt;\n" nt1
   <> indent <> printf "  if (nt_%s._ipg_end !== 0) {\n" nt1
   <> indent <> printf "    self._ipg_start = Math.min(self._ipg_start, left + nt_%s._ipg_start);\n" nt1
   <> indent <> printf "    self._ipg_end = Math.max(self._ipg_end, left + nt_%s._ipg_end);\n" nt1
   <> indent <> printf "    nt_%s._ipg_end += left;\n" nt1
   <> indent <> printf "    nt_%s._ipg_start += left;\n" nt1
   <> indent <>        "  }\n"
   <> indent <> printf "  self.values.push(nt_%s.%s);\n" nt1 i
   <> indent <>        "}\n\n"
  where es1 = argList env args1; es2 = argList env args2

alternativeToJS :: T -> Maybe T -> Env -> Alternative T T T Expr -> T
alternativeToJS indent parent env (Alternative ts subrules)
    = indent <>        "_ipg_alt: {\n"
   <> indent <>        "  let left; let right;\n"
   <>                     concatMap declare nts
   <> indent <> printf "  self = { %s_ipg_start: EOI, _ipg_end: 0 };\n\n" extend
   <>                     whereClauseToJS ("  " ++ indent) env subrules
   <>                     concatMap (termToJS ("  " ++ indent) env) ts
   <> indent <>        "  return self;\n"
   <> indent <>        "}\n"
  where nts = nonArrayNonTerminals ts
        declare nt = printf "%s  let nt_%s;\n" indent nt
        extend = case parent of Nothing -> ""; Just p -> printf "_ipg_parent: parent_of_%s, " p
    
ruleToJS :: Rule T T T Expr -> T
ruleToJS (Rule nt args alts)
    = printf "function %s(input%s) {\n" nt (paramList args)
   <>        "  const EOI = input.length; let self;\n"
   <>           concatMap (alternativeToJS "  " Nothing env) alts
   <>        "  return null;\n"
   <>        "}\n\n"
  where env = Set.fromList args

whereClauseToJS :: T -> Env -> Maybe (Grammar T T T Expr) -> T
whereClauseToJS indent env (Just (Grammar rules)) = concatMap (subruleToJS indent env) rules
whereClauseToJS _ _   Nothing = ""

subruleToJS :: T -> Env -> Rule T T T Expr -> T
subruleToJS indent env' (Rule nt args alts)
    = indent <> printf "const parent_of_%s = self;\n" nt
   <> indent <> printf "const %s = (input%s) => {\n" nt (paramList args)
   <> indent <>        "  const EOI = input.length; let self;\n"
   <>                     concatMap (alternativeToJS ("  " ++ indent) (Just nt) env) alts
   <> indent <>        "  return null;\n"
   <> indent <>        "};\n\n"
  where env = Set.union env' (Set.fromList args)

toJS :: Grammar T T T Expr -> T
toJS (Grammar rules)
    = "function _ipg_startsWith(s, prefix) {\n"
   <> "  if (typeof s === 'string') return s.startsWith(prefix);\n"
   <> "  if (s.length < prefix.length) return false;\n"
   <> "  for (let i = 0; i < prefix.length; ++i) {\n"
   <> "    if (s[i] !== prefix.charCodeAt(i)) return false;\n"
   <> "  }\n"
   <> "  return true;\n"
   <> "}\n\n"
   <> "function _ipg_lookup(env, i) {\n"
   <> "  let current = env;\n"
   <> "  while (!(i in current)) {\n"
   <> "    if (!('_ipg_parent' in current)) throw `Lookup of ${i} failed`;\n"
   <> "    current = current._ipg_parent;\n"
   <> "  }\n"
   <> "  return current[i];\n"
   <> "}\n\n"
   <> concatMap ruleToJS rules
