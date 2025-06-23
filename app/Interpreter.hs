module Interpreter ( interpret ) where
import Control.Applicative ( asum ) -- base
import Data.Bits ( shift, (.&.), (.|.), xor ) -- base
import qualified Data.Map as Map -- containers
import CoreIPG
import GenericExp ( Exp(..) )

type Buffer = String -- TODO: Switch to ByteString.

type Bindings id = Map.Map id (Value id)

data Value id
    = INT Integer
    | BOOL Boolean
    | FLOAT Double
    | STRING String
    | BINDINGS (Bindings id)
    | SEQUENCE [(Bindings id)]
  deriving ( Eq, Ord, Show )

type InterpFunc id = Bindings id -> Buffer -> Maybe (Bindings id, Int, Int)

type ExternalFunc id = [Value id] -> Value id
type ExternalFuncs t id = Map.Map t (ExternalFunc id)

data Context nt t id = Context {
    ruleFuncs :: Map.Map nt ([id], InterpFunc id),
    externalFuncs :: ExternalFuncs t id }

type Grammar' nt t id = Grammar nt t id (Exp t)
type Rule' nt t id = Rule nt t id (Exp t)
type Alternative' nt t id = Alternative nt t id (Exp t)
type Term' nt t id = Term nt t id (Exp t)

interpret
    :: (Ord nt, Ord id)
    => Grammar' nt t id
    -> ExternalFuncs t id
    -> Bindings id
    -> Buffer
    -> Maybe (Bindings id, Int, Int)
interpret (Grammar rules@(Rule _ startRule _ _:_) env_0 efs =
    (ruleFuncs ctxt Map.! startRule) env_0
  where ctxt = Context {
          ruleFuncs = Map.fromList (map (\rule@(Rule _ nt _ _) -> (nt, buildFunc rule)) rules),
          externalFuncs = efs }
        buildFunc = buildeInterpFunc ctxt -- Go, go knot tying.

buildInterpFunc :: (Ord nt, Ord id) => Context nt t id -> Rule' nt t id -> ([id], InterpFunc id)
buildInterpFunc ctxt (Rule _ nt args alts) =
    (args, \env buf -> asum (map (\interFunc -> interpFunc env buf) alts'))
  where alts' = map (interpAlt ctxt) alts

interpAlt :: (Ord nt, Ord id) => Context nt t id -> Alternative' nt t id -> InterpFunc id
interpAlt ctxt (Alternative terms) = \env buf -> go env buf terms'
  where terms' = map (interpTerm ctxt) terms
        go env buf [] = Just env
        go env buf (f:fs) =
            case f env buf of
                Nothing -> Nothing
                Just (env', start, end) -> go env' buf fs -- TODO: Do something with start/end

attr :: String -> String -> String
attr nt id = nt ++ '.':id

this :: String -> String
this nt = attr nt "this"

sTART :: String -> String
sTART nt = attr nt "START"

eND :: String -> String
eND nt = attr nt "END"

seq_ :: String -> String -> String
seq_ nt i = nt ++ '#':i

-- TODO: these, START, END
interpTerm :: (Ord nt, Ord id) => Context nt t id -> Term' nt t id -> InterpFunc id
interpTerm ctxt = go
  where go (NonTerminal nt args e_l e_r) = \env buf ->
            let eoi = length buf
                env' = Map.fromList (zip ids (map (eval ctxt eoi) args))
            in case (eval ctxt eoi e_l, eval ctxt eoi e_r) of
                (INT l, INT r) ->
                    case slice buf l r of
                        Nothing -> Nothing
                        Just buf' -> case rf env' buf' of
                            Nothing -> Nothing
                            Just (env'', _, 0) ->
                                Just (Map.insert (this nt) (BINDINGS env'') env, l, r)
                            Just (env'', l', r') ->
                                Just (Map.insert (this nt) (BINDINGS env'') env,
                                      l + l', l + r')
          where (ids, rf) = ruleFuncs ctxt Map.! nt
        go (Terminal t e_l e_r) = \env buf ->
            let eoi = length buf
            in case (eval ctxt eoi e_l, eval ctxt eoi e_r) of
                (INT l, INT r) ->
                    case slice buf l r of
                        Just buf' | t `isPrefixOf` buf' -> Just (env, l, l + length t)
                        _ -> Nothing
        go (id := e) = \env buf ->
            let eoi = length buf
            in Just (Map.insert id (eval ctxt eoi e) env, eoi, 0)
        go (Guard e) = \env buf ->
            let eoi = length buf
            in case eval ctxt eoi e of
                BOOL True -> Just (env, eoi, 0)
                _ -> Nothing
        go (Array id e e nt [e] e e) = \env buf -> -- TODO
            undefined -- for id=e_1 to e_2 do A(a_1, ..., a_m)[e_l, e_r]
        go (Any id e) = \env buf ->
            let eoi = length buf
            in case eval ctxt eoi e of
                INT i ->
                    case lookup i buf of
                        Just c -> Just (Map.insert id (INT (ord c)), i, i + 1)
                        Nothing -> Nothing
        go (Slice id e_l e_r) = \env buf ->
            let eoi = length buf
            in case (eval ctxt eoi e_l, eval ctxt eoi e_r) of
                (INT l, INT r) ->
                    case slice buf l r of
                        Just buf' -> Just (Map.insert id (STRING buf'), l, r)
                        _ -> Nothing
        go (Repeat nt [e] id) = \env buf -> -- TODO
            undefined -- repeat A(a_1, ..., a_m).id
        go (RepeatUntil nt [e] id nt [e]) = \env buf -> -- TODO
            undefined -- repeat A(a_1, ..., a_m).id until B(b_1, ..., b_k)

eval :: Context nt t id -> Int -> Bindings id -> Exp t -> Value id
eval ctxt eoi env = go
    where go (Int i) = INT i
          go (Float f) = FLOAT f
          go (String s) = STRING s
          go (Add l r) = add (go l) (go r)
              where add (INT x) (INT y) = INT (x + y)
                    add (FLOAT x) (FLOAT y) = FLOAT (x + y)
          go (Sub l r) = sub (go l) (go r)
              where sub (INT x) (INT y) = INT (x - y)
                    sub (FLOAT x) (FLOAT y) = FLOAT (x - y)
          go (Mul l r) = mul (go l) (go r)
              where mul (INT x) (INT y) = INT (x * y)
                    mul (FLOAT x) (FLOAT y) = FLOAT (x * y)
          go (Div l r) = div' (go l) (go r)
              where div' (INT x) (INT y) = INT (x `quot` y)
                    div' (FLOAT x) (FLOAT y) = FLOAT (x / y)
          go (Mod l r) = mod' (go l) (go r)
              where mod' (INT x) (INT y) = INT (x `rem` y)
          go (Exp l r) = exp' (go l) (go r)
              where exp' (FLOAT x) (FLOAT y) = FLOAT (x ** y)
                    exp' (INT x) (INT y) = INT (x ^ y)
          go (Neg l) = neg (go l)
              where neg (INT x) = INT (-x)
                    neg (FLOAT x) = FLOAT (-x)
          go (BitwiseNeg l) = bneg (go l)
              where bneg (INT x) = INT (complement x)
          go (Not l) = not' (go l)
              where not' (BOOL b) = not b
                    not' (INT i) = i == 0
          go (And l r) = and' (go l) (go r)
              where and' (BOOL x) (BOOL y) = x && y
                    and' (BOOL x) (INT y) = x && y /= 0
                    and' (INT x) (BOOL y) = x /= 0 && y
                    and' (INT x) (INT y) = x /= 0 && y /= 0
          go (Or l r) = or' (go l) (go r)
              where or' (BOOL x) (BOOL y) = x || y
                    or' (BOOL x) (INT y) = x || y /= 0
                    or' (INT x) (BOOL y) = x /= 0 || y
                    or' (INT x) (INT y) = x /= 0 || y /= 0
          go (BitwiseAnd l r) = band (go l) (go r)
              where band (INT x) (INT y) = INT (x .&. y)
          go (BitwiseXor l r) = bxor (go l) (go r)
              where bxor (INT x) (INT y) = INT (xor x y)
          go (BitwiseOr l r) = bor (go l) (go r)
              where bor (INT x) (INT y) = INT (x .|. y)
          go (LSh l r) = lsh (go l) (go r)
              where lsh (INT x) (INT y) = INT (shift x (fromIntegral y))
          go (RSh l r) = rsh (go l) (go r)
              where rsh (INT x) (INT y) = INT (shift x (-fromIntegral y))
          go (LessThan l r) = lessThan (go l) (go r)
              where lessThan (INT x) (INT y) = BOOL (x < y)
                    lessThan (FLOAT x) (FLOAT y) = BOOL (x < y)
                    lessThan (STRING x) (STRING y) = BOOL (x < y)
          go (LTE l r) = lte (go l) (go r)
              where lte (INT x) (INT y) = BOOL (x <= y)
                    lte (FLOAT x) (FLOAT y) = BOOL (x <= y)
                    lte (STRING x) (STRING y) = BOOL (x <= y)
          go (GreaterThan l r) = greaterThan (go l) (go r)
              where greaterThan (INT x) (INT y) = BOOL (x > y)
                    greaterThan (FLOAT x) (FLOAT y) = BOOL (x > y)
                    greaterThan (STRING x) (STRING y) = BOOL (x > y)
          go (GTE l r) = gte (go l) (go r)
              where gte (INT x) (INT y) = BOOL (x >= y)
                    gte (FLOAT x) (FLOAT y) = BOOL (x >= y)
                    gte (STRING x) (STRING y) = BOOL (x >= y)
          go (Equal l r) = equal (go l) (go r)
              where equal (INT x) (INT y) = BOOL (x == y)
                    equal (FLOAT x) (FLOAT y) = BOOL (x == y)
                    equal (STRING x) (STRING y) = BOOL (x == y)
          go (NotEqual l r) = notEqual (go l) (go r)
              where notEqual (INT x) (INT y) = BOOL (x /= y)
                    notEqual (FLOAT x) (FLOAT y) = BOOL (x /= y)
                    notEqual (STRING x) (STRING y) = BOOL (x /= y)
                    notEqual x y = NotEqual x y
          go (If b t e) = if_ (go b) (go t) (go e)
              where if_ (INT x) y z = if x /= 0 then y else z
                    if_ (BOOL b) y z = if b then y else z
          go (Call f es) = (externalFuncs ctxt Map.! f) es'
            where es' = map go es
          go (At l i) = at (go l) (go i)
              where at (SEQUENCE bs) (INT i) = BINDINGS (bs !! i)
          go (Ref (Id i)) = env Map.! i
          go (Ref (Attr nt i)) = env Map.! attr nt i
          go (Ref (Index nt e i)) = index (go e) (env Map.! seq_ nt i)
              where index (INT ix) (SEQUENCE bs) = BINDINGS (bs !! ix)
          go (Ref EOI) = INT (fromIntegral eoi)
          go (Ref (Start nt)) = env Map.! sTART nt
          go (Ref (End nt)) = env Map.! eND nt
