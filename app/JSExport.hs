module JSExport ( toJS, T ) where
import Data.Char ( isPrint, ord ) -- base
import qualified Data.Set as Set -- containers
import Numeric ( showHex ) -- base
import Data.List ( intersperse ) -- base
import Text.Printf ( printf ) -- base
import CoreIPG
import GenericExp ( Exp(..) )

-- TODO: Use Text rather than String.

-- It's worth noting that the way this export works already supports blackbox parsers.
-- ANY function that takes a slice and returns an object with the _ipg_start/_ipg_end fields
-- suitably set can just immediately be referenced. If we assume a blackbox parser will always
-- consume its full input, we can just return _ipg_start = 0, _ipg_end = input.length.

type T = String
type Expr = Exp T
type Env = Set.Set T

refToJS :: Env -> Ref T T Expr -> T
refToJS env (Id f) | f `Set.member` env = printf "a_%s" f
                   | otherwise          = printf "self.%s" f
refToJS _   (Attr nt "this") =
    printf "(({_ipg_start,_ipg_end,...o}) => o)(nt_%s)" nt
refToJS _   (Attr nt "these") =
    printf "seq_%s.map(({_ipg_start,_ipg_end,...o}) => o)" nt
refToJS _   (Attr nt f) = printf "nt_%s.%s" nt f
refToJS env (Index nt e "this") =
    printf "(({_ipg_start,_ipg_end,...o}) => o)(seq_%s[%s])" nt (exprToJS env e)
refToJS env (Index nt e f) = printf "seq_%s[%s].%s" nt (exprToJS env e) f
refToJS _   EOI = "EOI";
refToJS _   (Start nt) = printf "nt_%s._ipg_start" nt;
refToJS _   (End nt) = printf "nt_%s._ipg_end" nt;

exprToJS :: Env -> Expr -> T
exprToJS env e = exprToJS' env 0 e ""

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
    showParen (p > 17) (exprToJS' env 17 l . (".at("++) . exprToJS' env 0 r . (')':))
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
    where go c | isPrint c = [c] -- TODO: Pick something much narrower than this.
               | c `elem` "abfnrtv\\\"'" = show c
               | otherwise = '\\':'x':pad (showHex (ord c) "")
          pad h@[_] = '0':h
          pad h = h

-- left and right will be the interval *actually* consumed by the previous term if
-- it is a consuming term, otherwise it will be unchanged from earlier terms.
-- For Array, currently, we treat the "previous term" as the last iteration.
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
   <> indent <>        "}\n"
   <> indent <> printf "nt_%s._ipg_end += left;\n" nt
   <> indent <> printf "nt_%s._ipg_start += left;\n" nt
   <> indent <> printf "left = nt_%s._ipg_start;\n" nt
   <> indent <> printf "right = nt_%s._ipg_end;\n\n" nt
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
   <> indent <> printf "if (!input.slice(left, right).startsWith(%s)) break _ipg_alt;\n" terminal
   <> indent <>        "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>        "self._ipg_end = Math.max(self._ipg_end, right);\n"
   <> indent <> printf "right = left + %d;\n\n" (length t)
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
   <> indent <> printf "nt_%s = { _ipg_end: right, _ipg_start: left };\n" nt -- Special case
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
   <> indent <>        "  }\n"
   <> indent <>        "  tmp._ipg_end += left;\n"
   <> indent <>        "  tmp._ipg_start += left;\n"
   <> indent <> printf "  nt_%s._ipg_end = tmp._ipg_end;\n" nt -- Special case
   <> indent <> printf "  nt_%s._ipg_start = tmp._ipg_start;\n" nt -- Special case
   <> indent <> printf "  seq_%s.push(tmp);\n" nt
   <> indent <>        "}\n"
   <> indent <> printf "delete self.%s;\n" i
   <> indent <> printf "left = nt_%s._ipg_start;\n" nt
   <> indent <> printf "right = nt_%s._ipg_end;\n\n" nt
  where startExp = exprToJS env start; endExp = exprToJS' env 10 end "";
        lExp = exprToJS env l; rExp = exprToJS env r; es = argList env args
termToJS indent env (Any i l)
    = indent <> printf "// {%s = .[%s]}\n" i lExp
   <> indent <> printf "left = %s;\n" lExp
   <> indent <>        "right = left + 1;\n"
   <> indent <>        "if (left < 0 || right > EOI) break _ipg_alt;\n"
   <> indent <> printf "self.%s = input.at(left);\n" i
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
termToJS indent env (Repeat nt args i)
    = indent <> printf "// repeat %s%s.%s\n" nt (call es) i
   <> indent <>        "self.values = [];\n"
   <> indent <> printf "nt_%s = %s(input.slice(right, EOI)%s);\n" nt nt es
   <> indent <> printf "if (nt_%s !== null) {\n" nt
   <> indent <> printf "  if (nt_%s._ipg_end === 0) throw 'repeat of non-consuming rule: %s';\n" nt nt
   <> indent <> printf "  self._ipg_start = Math.min(self._ipg_start, right + nt_%s._ipg_start);\n" nt
   <> indent <> printf "  self._ipg_end = Math.max(self._ipg_end, right + nt_%s._ipg_end);\n" nt
   <> indent <> printf "  nt_%s._ipg_end += right;\n" nt
   <> indent <> printf "  nt_%s._ipg_start += right;\n" nt
   <> indent <> printf "  left = nt_%s._ipg_start;\n" nt
   <> indent <> printf "  right = nt_%s._ipg_end;\n" nt
   <> indent <> printf "  self.values.push(nt_%s.%s);\n\n" nt i

   <> indent <>        "  while(right <= EOI) {\n"
   <> indent <> printf "    nt_%s = %s(input.slice(right, EOI)%s);\n" nt nt es
   <> indent <> printf "    if (nt_%s === null) break;\n" nt
   <> indent <> printf "    if (nt_%s._ipg_end === 0) throw 'repeat of non-consuming rule: %s';\n" nt nt
   <> indent <> printf "    self._ipg_start = Math.min(self._ipg_start, right + nt_%s._ipg_start);\n" nt
   <> indent <> printf "    self._ipg_end = Math.max(self._ipg_end, right + nt_%s._ipg_end);\n" nt
   <> indent <> printf "    nt_%s._ipg_end += right;\n" nt
   <> indent <> printf "    nt_%s._ipg_start += right;\n" nt
   <> indent <> printf "    self.values.push(nt_%s.%s);\n" nt i
   <> indent <> printf "    right = nt_%s._ipg_end;\n" nt
   <> indent <>        "  }\n"
   <> indent <>        "}\n\n"
  where es = argList env args
termToJS indent env (RepeatUntil nt1 args1 i nt2 args2)
    = indent <> printf "// repeat %s%s.%s until %s%s\n" nt1 (call es1) i nt2 (call es2)
   <> indent <>        "left = right;\n"
   <> indent <>        "self.values = [];\n"
   <> indent <>        "while(true) {\n"
   <> indent <>        "  if (EOI < right) break _ipg_alt;\n"
   <> indent <> printf "  nt_%s = %s(input.slice(right, EOI)%s);\n" nt2 nt2 es2
   <> indent <> printf "  if (nt_%s !== null) {\n" nt2
   <> indent <> printf "    if (nt_%s._ipg_end !== 0) {\n" nt2
   <> indent <> printf "      self._ipg_start = Math.min(self._ipg_start, right + nt_%s._ipg_start);\n" nt2
   <> indent <> printf "      self._ipg_end = Math.max(self._ipg_end, right + nt_%s._ipg_end);\n" nt2
   <> indent <>        "    }\n"
   <> indent <> printf "    nt_%s._ipg_end += right;\n" nt2
   <> indent <> printf "    nt_%s._ipg_start += right;\n" nt2
   <> indent <> printf "    right = nt_%s._ipg_end;\n" nt2
   <> indent <>        "    break;\n"
   <> indent <>        "  }\n"
   <> indent <> printf "  nt_%s = %s(input.slice(right, EOI)%s);\n" nt1 nt1 es1
   <> indent <> printf "  if (nt_%s === null) break _ipg_alt;\n" nt1
   <> indent <> printf "  if (nt_%s._ipg_end === 0) throw 'repeat of non-consuming rule: %s';\n" nt1 nt1
   <> indent <> printf "  self._ipg_start = Math.min(self._ipg_start, right + nt_%s._ipg_start);\n" nt1
   <> indent <> printf "  self._ipg_end = Math.max(self._ipg_end, right + nt_%s._ipg_end);\n" nt1
   <> indent <> printf "  nt_%s._ipg_end += right;\n" nt1
   <> indent <> printf "  nt_%s._ipg_start += right;\n" nt1
   <> indent <> printf "  self.values.push(nt_%s.%s);\n" nt1 i
   <> indent <> printf "  right = nt_%s._ipg_end;\n" nt1
   <> indent <>        "}\n\n"
  where es1 = argList env args1; es2 = argList env args2

alternativeToJS :: Maybe (T, [T]) -> T -> Env -> Alternative T T T Expr -> T
alternativeToJS instrument indent env (Alternative ts)
    = indent <>        "_ipg_alt: {\n"
   <> indent <>        "  let left = EOI; let right = 0;\n"
   <>                     concatMap declare nts
   <>                     concatMap declareSeqs seqs
   <> indent <>        "  self = { _ipg_start: EOI, _ipg_end: 0 };\n\n"
   <>                     concatMap (termToJS ("  " ++ indent) env) ts
   <>                     instrumentation
   <> indent <>        "  return self;\n"
   <> indent <>        "}\n"
  where nts = nonTerminals ts
        seqs = arrayNonTerminals ts
        declare nt = printf "%s  let nt_%s;\n" indent nt
        declareSeqs nt = printf "%s  let seq_%s;\n" indent nt
        instrumentation = case instrument of
                            Nothing -> ""
                            -- Purposely over-indented.
                            Just (nt, args) -> indent <>
                                printf "    console.error({%s: self%s});\n" nt (paramList args)
    
ruleToJS :: Rule T T T Expr -> T
ruleToJS (Rule mt nt args alts)
    = printf "function %s(input%s) {\n" nt (paramList args)
   <>        "  const EOI = input.length; let self;\n"
   <>           concatMap (alternativeToJS instrument "  " env) alts
   <>        "  return null;\n"
   <>        "}\n\n"
  where env = Set.fromList args
        instrument = if INSTRUMENT `elem` mt then Just (nt, args) else Nothing

toJS :: Grammar T T T Expr -> T
toJS (Grammar rules)
    = "class ByteSlice {\n"
   <> "  constructor(bytes, start = 0, end = bytes.length) {\n"
   <> "    this.data = bytes; this.start = start; this.length = end - start;\n"
   <> "  }\n"
   <> "  at(ix) { return this.data[this.start + ix]; }\n" -- TODO: Do bounds checking.
   <> "  slice(l, r) { return new ByteSlice(this.data, this.start + l, this.start + r); }\n"
   <> "  startsWith(prefix) {\n"
   <> "    if (this.length < prefix.length) return false;\n"
   <> "    for (let i = 0; i < prefix.length; ++i) {\n"
   <> "      if (this.at(i) !== prefix.charCodeAt(i)) return false;\n"
   <> "    }\n"
   <> "    return true;\n"
   <> "  }\n"
   <> "  copy() { return this.data.slice(this.start, this.start + this.length); }\n"
   <> "}\n\n"
   <> concatMap ruleToJS rules
