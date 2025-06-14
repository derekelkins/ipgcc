module CoreIPG ( 
    Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..), CrudeExpr(..),

    toJS, 

    Expr(..), T(..), -- TODO: Temporarily.
) where
import Data.Foldable ( concatMap ) -- base
import Text.Printf ( printf ) -- base

-- A CrudeExpr is essentially a templated string.
-- For example, something like:
--   CrudeExpr (\case 0 -> "foo") [Left "mul(", Right 0, Left ", ", Right 0, Left ")"]
-- would correspond to something like `mul(${foo}, ${foo})`.
data CrudeExpr r s = CrudeExpr (Int -> r) [Either Int s]

newtype Grammar nt t id e = Grammar [Rule nt t id e]
    deriving ( Show )

data Rule nt t id e = Rule nt [Alternative nt t id e] -- A -> alt_1 / ... / alt_n
    deriving ( Show )

newtype Alternative nt t id e = Alternative [Term nt t id e] -- tm_1 ... tm_n
    deriving ( Show )

data Term nt t id e 
    = NonTerminal nt e e  -- A[e_l, e_r]
    | Terminal t e e      -- s[e_l, e_r]
    | id := e             -- {id = e}
    | Guard e             -- <e>
    | Array id e e nt e e -- for id=e_1 to e_2 do A[e_l, e_r]
    | Any id e            -- {id = input[e]}
  deriving ( Show )
    
data Ref id nt e
    = Id id               -- id, essentially self.id
    | Attr nt id          -- A.id
    | Index nt e id       -- A(e).id
    | EOI                 -- EOI
    | Start nt            -- A.start
    | End nt              -- A.end
  deriving ( Show )


-- TODO: Move the below to a different module.
-- TODO: Use Text rather than String.

type T = String
newtype Expr = Expr { unExpr :: CrudeExpr (Ref T T Expr) T }

refToJS :: Ref T T Expr -> T
refToJS (Id f) = printf "_ipg_self.%s" f
refToJS (Attr nt f) = printf "_ipg_%s.%s" nt f
refToJS (Index nt e f) = printf "_ipg_seq_%s[%s].%s" nt (exprToJS e) f
refToJS EOI = "_ipg_end";
refToJS (Start nt) = printf "_ipg_%s._ipg_start" nt;
refToJS (End nt) = printf "_ipg_%s._ipg_end" nt;

exprToJS :: Expr -> T
exprToJS (Expr (CrudeExpr subst parts)) = concatMap go parts
    where go (Left ix) = refToJS (subst ix)
          go (Right s) = s

termToJS :: Term T T T Expr -> T
termToJS (NonTerminal nt l r)
    = printf "    _ipg_%s = %s(_ipg_input, _ipg_start + (%s), _ipg_start + (%s));\n"
        nt nt (exprToJS l) (exprToJS r)
   <> printf "    if (_ipg_%s === null) break _ipg_alt;\n" nt
   <> printf "    _ipg_self._ipg_end = _ipg_%s._ipg_end;\n" nt
termToJS (Terminal t l r)
    = printf "    if (\"%s\" !== _ipg_input.slice(_ipg_start + (%s), _ipg_start + (%s))) break _ipg_alt;\n"
        t (exprToJS l) (exprToJS r) -- TODO: Better escaping.
   <> printf "    _ipg_self._ipg_end = _ipg_start + (%s);\n" (exprToJS r) -- TODO: Don't recalculate r.
termToJS (i := e) = printf "    _ipg_self.%s = (%s);\n" i (exprToJS e)
termToJS (Guard e) = printf "    if (!(%s)) break _ipg_alt;\n" (exprToJS e)
termToJS (Array i start end nt l r)
    = printf "    _ipg_seq_%s = [];\n" nt
   <> printf "    for (_ipg_self.%s = (%s); _ipg_self.%s < (%s); _ipg_self.%s++) {\n"
            i (exprToJS start) i (exprToJS end) i
   <> printf "      const _ipg_tmp = %s(_ipg_input, _ipg_start + (%s), _ipg_start + (%s));\n"
            nt (exprToJS l) (exprToJS r)
   <>        "      if (_ipg_tmp === null) break _ipg_alt;\n"
   <>        "      _ipg_self._ipg_end = _ipg_tmp._ipg_end;\n"
   <> printf "      _ipg_seq_%s.push(_ipg_tmp);\n" nt
   <>        "    }\n"
termToJS (Any i e)
    = printf "    { const _ipg_tmp = _ipg_input[_ipg_start + (%s)];\n" (exprToJS e)
   <>        "      if (_ipg_tmp === undefined) break _ipg_alt;\n"
   <> printf "      _ipg_self._ipg_end = _ipg_start + (%s) + 1;\n" (exprToJS e) -- TODO: Don't recalculate e.
   <> printf "      _ipg_self.%s = _ipg_tmp; }\n" i

-- TODO: Declare the non-terminals occuring in the terms.
alternativeToJS :: Alternative T T T Expr -> T
alternativeToJS (Alternative ts)
    = "  _ipg_alt: {\n"
   <> "    _ipg_self = { _ipg_start, _ipg_end: _ipg_start };\n"
   <>      concatMap termToJS ts
   <> "    return _ipg_self;\n"
   <> "  }\n"
    
ruleToJS :: Rule T T T Expr -> T
ruleToJS (Rule nt alts)
    = printf "function %s(_ipg_input, _ipg_start, _ipg_end) {\n" nt
          <> "  let _ipg_self = { _ipg_start, _ipg_end: _ipg_start };\n"
          <> concatMap alternativeToJS alts
          <> "  return null;\n}\n\n"

toJS :: Grammar T T T Expr -> T
toJS (Grammar rules) = concatMap ruleToJS rules
