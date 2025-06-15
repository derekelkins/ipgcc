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
refToJS (Id f) = printf "_ipg_self.%s" f
refToJS (Attr nt f) = printf "_ipg_nt_%s.%s" nt f
refToJS (Index nt e f) = printf "_ipg_seq_%s[%s].%s" nt (exprToJS e) f
refToJS EOI = "_ipg_eoi";
refToJS (Start nt) = printf "_ipg_nt_%s._ipg_start" nt;
refToJS (End nt) = printf "_ipg_nt_%s._ipg_end" nt;

-- TODO: Make this a pretty printer that omits parens when it can.
exprToJS :: Expr -> T
exprToJS e = exprToJS' e ""

exprToJS' :: Expr -> T -> T
exprToJS' (Int n) = shows n
exprToJS' (Float n) = shows n
exprToJS' (String s) = shows s
exprToJS' (Add l r) = ('(':) . exprToJS' l . ('+':) . exprToJS' r . (')':)
exprToJS' (Sub l r) = ('(':) . exprToJS' l . ('-':) . exprToJS' r . (')':)
exprToJS' (Mul l r) = ('(':) . exprToJS' l . ('*':) . exprToJS' r . (')':)
exprToJS' (Div l r) = ('(':) . exprToJS' l . ('/':) . exprToJS' r . (')':)
exprToJS' (Neg e) = ('-':) . exprToJS' e
exprToJS' (And l r) = ('(':) . exprToJS' l . ("&&"++) . exprToJS' r . (')':)
exprToJS' (Or l r) = ('(':) . exprToJS' l . ("||"++) . exprToJS' r . (')':)
exprToJS' (LSh l r) = ('(':) . exprToJS' l . ("<<"++) . exprToJS' r . (')':)
exprToJS' (RSh l r) = ('(':) . exprToJS' l . (">>"++) . exprToJS' r . (')':)
exprToJS' (LessThan l r) = ('(':) . exprToJS' l . ('<':) . exprToJS' r . (')':)
exprToJS' (LTE l r) = ('(':) . exprToJS' l . ("<="++) . exprToJS' r . (')':)
exprToJS' (GreaterThan l r) = ('(':) . exprToJS' l . ('>':) . exprToJS' r . (')':)
exprToJS' (GTE l r) = ('(':) . exprToJS' l . (">="++) . exprToJS' r . (')':)
exprToJS' (Equal l r) = ('(':) . exprToJS' l . ('=':) . exprToJS' r . (')':)
exprToJS' (If b t e) = ('(':) . exprToJS' b . ('?':) . exprToJS' t . (':':) . exprToJS' e . (')':)
exprToJS' (Call t es) =
    shows t . ('(':) . foldr (.) id (intersperse (',':) $ map exprToJS' es) . (')':)
exprToJS' (At l r) = exprToJS' l . ('[':) . exprToJS' r . (']':)
exprToJS' (Ref r) = (refToJS r++)

termToJS :: Term T T T Expr -> T
termToJS (NonTerminal nt l r)
    = printf "    // %s[%s, %s]\n" nt lExp rExp
   <> printf "    _ipg_left = (%s);\n" lExp
   <> printf "    _ipg_right = (%s);\n" rExp
   <>        "    if (_ipg_left < 0 || _ipg_right < _ipg_left || _ipg_right > _ipg_eoi) break _ipg_alt;\n"
   <> printf "    _ipg_nt_%s = %s(_ipg_input.slice(_ipg_left, _ipg_right));\n" nt nt
   <> printf "    if (_ipg_nt_%s === null) break _ipg_alt;\n" nt
   <> printf "    if (_ipg_nt_%s._ipg_end !== 0) {\n" nt
   <> printf "      _ipg_self._ipg_start = Math.min(_ipg_self._ipg_start, _ipg_left + _ipg_nt_%s._ipg_start);\n" nt
   <> printf "      _ipg_self._ipg_end = Math.max(_ipg_self._ipg_end, _ipg_left + _ipg_nt_%s._ipg_end);\n" nt
   <> printf "      _ipg_nt_%s._ipg_end += _ipg_left;\n" nt
   <> printf "      _ipg_nt_%s._ipg_start += _ipg_left;\n" nt
   <>        "    }\n\n"
  where lExp = exprToJS l; rExp = exprToJS r
termToJS (Terminal "" l r) 
    = printf "    // epsilon[%s, %s]\n" lExp rExp
   <> printf "    _ipg_left = (%s);\n" lExp
   <> printf "    _ipg_right = (%s);\n" rExp
   <>        "    if (_ipg_left < 0 || _ipg_right < _ipg_left || _ipg_right > _ipg_eoi) break _ipg_alt;\n\n"
  where lExp = exprToJS l; rExp = exprToJS r
termToJS (Terminal t l r)
    = printf "    // %s[%s, %s]\n" (show t) lExp rExp
   <> printf "    _ipg_left = (%s);\n" lExp
   <> printf "    _ipg_right = (%s);\n" rExp
   <>        "    if (_ipg_left < 0 || _ipg_right < _ipg_left || _ipg_right > _ipg_eoi) break _ipg_alt;\n"
   <> printf "    if (!_ipg_input.slice(_ipg_left, _ipg_right).startsWith(%s)) break _ipg_alt;\n" (show t)
   <>        "    _ipg_self._ipg_start = Math.min(_ipg_self._ipg_start, _ipg_left);\n"
   <>        "    _ipg_self._ipg_end = Math.max(_ipg_self._ipg_end, _ipg_right);\n\n"
  where lExp = exprToJS l; rExp = exprToJS r
termToJS (i := e)
    = printf "    // {%s = %s}\n" i eExp
   <> printf "    _ipg_self.%s = (%s);\n\n" i eExp
  where eExp = exprToJS e
termToJS (Guard e)
    = printf "    // <%s>\n" eExp
   <> printf "    if (!(%s)) break _ipg_alt;\n\n" eExp
  where eExp = exprToJS e
termToJS (Array i start end nt l r)
    = printf "    // for %s = %s to %s do %s[%s, %s]\n" i startExp endExp nt lExp rExp
   <> printf "    _ipg_seq_%s = [];\n" nt
   <> printf "    for (_ipg_self.%s = (%s); _ipg_self.%s < (%s); _ipg_self.%s++) {\n"
            i startExp i endExp i
   <> printf "      const _ipg_left = (%s);\n" lExp
   <> printf "      const _ipg_right = (%s);\n" rExp
   <>        "      if (_ipg_left < 0 || _ipg_right < _ipg_left || _ipg_right > _ipg_eoi) break _ipg_alt;\n"
   <> printf "      const _ipg_tmp = %s(_ipg_input.slice(_ipg_left, _ipg_right));\n" nt
   <>        "      if (_ipg_tmp === null) break _ipg_alt;\n"
   <>        "      if (_ipg_tmp._ipg_end !== 0) {\n"
   <>        "        _ipg_self._ipg_start = Math.min(_ipg_self._ipg_start, _ipg_left + _ipg_tmp._ipg_start);\n"
   <>        "        _ipg_self._ipg_end = Math.max(_ipg_self._ipg_end, _ipg_left + _ipg_tmp._ipg_end);\n"
   <>        "        _ipg_tmp._ipg_end += _ipg_left;\n"
   <>        "        _ipg_tmp._ipg_start += _ipg_left;\n"
   <>        "       }\n"
   <> printf "      _ipg_seq_%s.push(_ipg_tmp);\n" nt
   <>        "    }\n"
   <> printf "    delete _ipg_self.%s;\n\n" i
  where startExp = exprToJS start; endExp = exprToJS end; lExp = exprToJS l; rExp = exprToJS r
termToJS (Any i l)
    = printf "    // {%s = s[%s]}\n" i lExp
   <> printf "    _ipg_left = (%s);\n" lExp
   <>        "    _ipg_right = _ipg_left + 1;\n"
   <>        "    if (_ipg_left < 0 || _ipg_right > _ipg_eoi) break _ipg_alt;\n"
   <> printf "    _ipg_self.%s = _ipg_input[_ipg_left];\n" i
   <>        "    _ipg_self._ipg_start = Math.min(_ipg_self._ipg_start, _ipg_left);\n"
   <>        "    _ipg_self._ipg_end = Math.max(_ipg_self._ipg_end, _ipg_right);\n\n"
  where lExp = exprToJS l
termToJS (Slice i l r)
    = printf "    // {%s = s[%s, %s]}\n" i lExp rExp
   <> printf "    _ipg_left = (%s);\n" lExp
   <> printf "    _ipg_right = (%s);\n" rExp
   <>        "    if (_ipg_left < 0 || _ipg_right < _ipg_left || _ipg_right > _ipg_eoi) break _ipg_alt;\n"
   <> printf "    _ipg_self.%s = _ipg_input.slice(_ipg_left, _ipg_right);\n" i
   <>        "    if (_ipg_left !== _ipg_right) {\n"
   <>        "      _ipg_self._ipg_start = Math.min(_ipg_self._ipg_start, _ipg_left);\n"
   <>        "      _ipg_self._ipg_end = Math.max(_ipg_self._ipg_end, _ipg_right);\n"
   <>        "    }\n\n"
  where lExp = exprToJS l; rExp = exprToJS r

alternativeToJS :: Alternative T T T Expr -> T
alternativeToJS (Alternative ts)
    = "  _ipg_alt: {\n"
   <> "    let _ipg_left; let _ipg_right;\n"
   <>      concatMap declare nts
   <> "    _ipg_self = { _ipg_start: _ipg_eoi, _ipg_end: 0 };\n\n"
   <>      concatMap termToJS ts
   <> "    return _ipg_self;\n"
   <> "  }\n"
  where nts = nonArrayNonTerminals ts
        declare nt = printf "    let _ipg_nt_%s;\n" nt
    
ruleToJS :: Rule T T T Expr -> T
ruleToJS (Rule nt alts)
    = printf "function %s(_ipg_input) {\n" nt
          <> "  const _ipg_eoi = _ipg_input.length;\n"
          <> "  let _ipg_self = { _ipg_start: _ipg_eoi, _ipg_end: 0 };\n"
          <> concatMap alternativeToJS alts
          <> "  return null;\n}\n\n"

toJS :: Grammar T T T Expr -> T
toJS (Grammar rules) = concatMap ruleToJS rules
