{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Text.IPG.Interpreter (
    Bindings, Buffer, Id, NT, Value(..),
    asJSON, asJSON', interpret, interpretStartingWith,
) where
import Control.Applicative ( asum ) -- base
import Data.Bits ( complement, shift, xor, (.&.), (.|.) ) -- base
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import Data.Either ( lefts, partitionEithers ) -- base
import Data.Int ( Int64 ) -- base
import Data.List ( (!?), intersperse ) -- base
import qualified Data.Map as Map -- containers
import GHC.Stack ( HasCallStack ) -- base

import Text.IPG.Core ( Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..) )
import Text.IPG.GenericExp ( UnOp(..), BinOp(..), Exp(..) )
import Text.IPG.PPrint ( hexyString )

(!!!) :: (HasCallStack, Ord k, Show k) => Map.Map k v -> k -> v
m !!! k = case Map.lookup k m of
            Nothing -> error (show k ++ " is not in Map")
            Just v -> v

(!!.) :: HasCallStack => [v] -> Int -> v
l !!. ix = case l !? ix of
            Nothing -> error (show ix ++ " is out of bounds")
            Just v -> v

type Id = BS.ByteString
type T = BS.ByteString
type NT = BS.ByteString

type Buffer = BS.ByteString

type Bindings a = Map.Map Id (Value a)

type Parameters a = Map.Map Id (Value a)

data EnvEntry a = EnvEntry {
    start_ :: Int,
    end_ :: Int,
    this_ :: Bindings a,
    these_ :: [Bindings a],
    offset_ :: Int
  }

type NTBindings a = Map.Map (NT, Int) (EnvEntry a)

type Environment a = (NTBindings a, Bindings a)

data Value a
    = INT !Int64
    | BOOL !Bool
    | FLOAT !Double
    | STRING !Buffer
    | BINDINGS (Bindings a)
    | SEQUENCE [Value a]
    | OPAQUE a
  deriving ( Eq, Ord, Show )

asJSON' :: Maybe (Value a) -> Builder.Builder
asJSON' (Just v) = asJSON v
asJSON' Nothing = "null"

asJSON :: Value a -> Builder.Builder
asJSON (STRING s) = hexyString s
asJSON (BOOL True) = "true"
asJSON (BOOL False) = "false"
asJSON (INT n) = Builder.int64Dec n
asJSON (FLOAT d) = Builder.doubleDec d
asJSON (SEQUENCE xs) = "[" <> mconcat (intersperse ", " (map asJSON xs)) <> "]"
asJSON (BINDINGS xs) = "{" <> mconcat (intersperse ", " (map process (Map.toList xs))) <> "}"
    where process (k, v) = hexyString k <> ": " <> asJSON v

type InterpFunc a = Environment a -> Parameters a -> Buffer -> Maybe (Environment a, Int, Int)

type ExternalFunc a = [Value a] -> Value a
type ExternalFuncs a = Map.Map NT (ExternalFunc a)

data Context a = Context {
    ruleFuncs :: Map.Map NT ([Id], InterpFunc a),
    constants :: Map.Map Id (Value a),
    externalFuncs :: ExternalFuncs a }

type Exp' = Exp NT T Id
type Grammar' = Grammar NT T Id Exp'
type Rule' = Rule NT T Id Exp'
type Alternative' = Alternative NT T Id Exp'
type Term' = Term NT T Id Exp'

interpret
    :: (HasCallStack)
    => Grammar'
    -> ExternalFuncs a
    -> [Value a]
    -> Buffer
    -> Maybe (Bindings a, Int, Int)
interpret g@(Grammar ruleOrConsts) efs = interpretStartingWith g efs startRule
    where (Rule _ startRule _ _:_) = lefts ruleOrConsts

interpretStartingWith
    :: (HasCallStack)
    => Grammar'
    -> ExternalFuncs a
    -> NT
    -> [Value a]
    -> Buffer
    -> Maybe (Bindings a, Int, Int)
interpretStartingWith (Grammar ruleOrConsts) efs startRule args =
    let (params, body) = ruleFuncs ctxt !!! startRule
        args' = Map.fromList (zip params args)
    in fmap (\((_, bindings), s, e) -> (bindings, s, e)) . body (Map.empty, Map.empty) args'
  where ctxt = Context {
          ruleFuncs = Map.fromList (map (\rule@(Rule _ nt _ _) -> (nt, buildFunc rule)) rules),
          constants = Map.fromList (map (fmap eval') consts),
          externalFuncs = efs }
        buildFunc = buildInterpFunc ctxt -- Go, go knot tying.
        eval' = eval ctxt (error "EOI in const") (Map.empty, Map.empty) Map.empty
        (rules, consts) = partitionEithers ruleOrConsts

buildInterpFunc :: (HasCallStack) => Context a -> Rule' -> ([Id], InterpFunc a)
buildInterpFunc ctxt (Rule _ _ args alts) =
    (args, \env ps buf -> asum (map (\interpFunc -> interpFunc env ps buf) alts'))
  where alts' = map (interpAlt ctxt) alts

interpAlt :: (HasCallStack) => Context a -> Alternative' -> InterpFunc a
interpAlt ctxt (Alternative terms) = \env ps buf -> go env ps buf (BS.length buf) 0 0 terms'
  where terms' = map (interpTerm ctxt) terms
        go env _ _ minStart maxEnd _ [] = Just (env, minStart, maxEnd)
        go env ps buf minStart maxEnd prev (f:fs) =
            case f prev env ps buf of
                Nothing -> Nothing
                Just (env', start, end) ->
                    go env' ps buf (min start minStart) (max end maxEnd) end fs

slice :: Int -> Int -> Buffer -> Maybe Buffer
slice l r buf | 0 <= l && l <= r && r <= BS.length buf = Just (BS.take (r - l) (BS.drop l buf))
              | otherwise = Nothing

interpNonTerminal
    :: (HasCallStack)
    => Context a
    -> (NT, Int)
    -> [Exp']
    -> Exp'
    -> Exp'
    -> Int
    -> InterpFunc a
interpNonTerminal ctxt nt args e_l e_r = \_ env@(ntbs, bs) ps buf ->
    let eoi = BS.length buf
        args' = Map.fromList (zip ids (map (eval ctxt eoi env ps) args))
    in case (eval ctxt eoi env ps e_l, eval ctxt eoi env ps e_r) of
        (INT l, INT r) ->
            case slice (fromIntegral l) (fromIntegral r) buf of
                Nothing -> Nothing
                Just buf' -> case rf (Map.empty, Map.empty) args' buf' of
                    Nothing -> Nothing
                    Just ((_, bs''), l', r') ->
                        let start = fromIntegral l + l'
                            end = fromIntegral l + r'
                            initialEntry =
                                EnvEntry {
                                    start_ = start,
                                    end_ = end,
                                    this_ = bs'',
                                    these_ = [],
                                    offset_ = 0
                                }
                            ntbs'' = Map.insertWith const nt initialEntry ntbs
                        in Just ((ntbs'', bs), start, end)
  where (ids, rf) = ruleFuncs ctxt !!! fst nt

interpTerm :: (HasCallStack) => Context a -> Term' -> Int -> InterpFunc a
interpTerm ctxt = go
  where go (NonTerminal nt args e_l e_r) = interpNonTerminal ctxt nt args e_l e_r
        go (Terminal "" e_l e_r) = \_ env ps buf ->
            let eoi = BS.length buf
            in case (eval ctxt eoi env ps e_l, eval ctxt eoi env ps e_r) of
                (INT l, INT r) ->
                    case slice (fromIntegral l) (fromIntegral r) buf of
                        Just _ -> Just (env, eoi, 0)
                        _ -> Nothing
        go (Terminal t e_l e_r) = \_ env ps buf ->
            let eoi = BS.length buf
            in case (eval ctxt eoi env ps e_l, eval ctxt eoi env ps e_r) of
                (INT l, INT r) ->
                    case slice (fromIntegral l) (fromIntegral r) buf of
                        Just buf' | t `BS.isPrefixOf` buf' ->
                            Just (env, fromIntegral l, fromIntegral l + BS.length t)
                        _ -> Nothing
        go (x := e) = \_ env@(ntbs, bs) ps buf ->
            let eoi = BS.length buf
            in Just ((ntbs, Map.insert x (eval ctxt eoi env ps e) bs), eoi, 0)
        go (Guard e) = \_ env ps buf ->
            let eoi = BS.length buf
            in case eval ctxt eoi env ps e of
                BOOL True -> Just (env, eoi, 0)
                _ -> Nothing
        go (Array j s e nt es e_l e_r) = \start env@(ntbs, bs) ps buf ->
            let eoi = BS.length buf
            in fmap (\(env', _, end) -> (env', start, end))
                  (case (eval ctxt eoi env ps s, eval ctxt eoi env ps e) of
                    (INT s', INT e') ->
                        let offset = fromIntegral s' :: Int
                            initialEntry =
                                EnvEntry { -- TODO
                                    start_ = error "Array term can't use A.START unless A has already occurred",
                                    end_ = start,
                                    this_ = error "Array term can't use A.this unless A has already occurred",
                                    these_ = [],
                                    offset_ = offset
                                }
                            ntbs' = Map.insertWith (\_ old -> old) nt initialEntry ntbs
                        in loop [] offset (fromIntegral e') start (ntbs', bs) ps buf)
          where loop acc s' e' prev env@(ntbs, bs) ps buf
                    | s' < e' =
                        case body prev env (Map.insert j (INT (fromIntegral s')) ps) buf of
                            Nothing -> Nothing
                            Just ((ntbs', bs'), _, end') ->
                                loop (this_ (ntbs' !!! nt):acc) (s' + 1) e'
                                    end' (ntbs', bs') ps buf
                    | otherwise =
                        Just ((Map.adjust (\ee -> ee { these_ = reverse acc }) nt ntbs, bs),
                              prev,
                              prev)
                body = interpNonTerminal ctxt nt es e_l e_r
        go (Any x e) = \_ env@(ntbs, bs) ps buf ->
            let eoi = BS.length buf
            in case eval ctxt eoi env ps e of
                INT ix ->
                    case BS.indexMaybe buf (fromIntegral ix) of
                        Just c ->
                            Just ((ntbs, Map.insert x (INT (fromIntegral c)) bs),
                                  fromIntegral ix,
                                  fromIntegral ix + 1)
                        _ -> Nothing
        go (Slice x e_l e_r) = \_ env@(ntbs, bs) ps buf ->
            let eoi = BS.length buf
            in case (eval ctxt eoi env ps e_l, eval ctxt eoi env ps e_r) of
                (INT l, INT r) ->
                    case slice (fromIntegral l) (fromIntegral r) buf of
                        Just buf' ->
                            Just ((ntbs, Map.insert x (STRING buf') bs),
                                  fromIntegral l,
                                  fromIntegral r)
                        _ -> Nothing
        go (Repeat nt es l r x l0 r0) = \start env@(ntbs, bs) ps buf ->
            let eoi = BS.length buf
            in case body0 start env ps buf of
                Nothing -> Just ((ntbs, Map.insert "values" (SEQUENCE []) bs), eoi, 0)
                Just ((ntbs', bs'), start', end') ->
                        loop [access ntbs' nt x] start' end' end' (ntbs', bs') ps buf
          where loop acc minStart maxEnd prev env@(ntbs, bs) ps buf =
                    case body prev env ps buf of
                        Nothing ->
                            Just ((ntbs, Map.insert "values" (SEQUENCE (reverse acc)) bs),
                                  minStart,
                                  maxEnd)
                        Just (env'@(ntbs', _), start', end') ->
                            loop (access ntbs' nt x:acc)
                                 (min start' minStart)
                                 (max end' maxEnd)
                                 end'
                                 env'
                                 ps
                                 buf
                body0 = interpNonTerminal ctxt nt es l0 r0
                body = interpNonTerminal ctxt nt es l r
        go (RepeatUntil nt1 es1 l r x l0 r0 nt2 es2) = \start env ps buf ->
            case condition0 start env ps buf of
                Just ((ntbs', bs'), start', end') ->
                    Just ((ntbs', Map.insert "values" (SEQUENCE []) bs'), start', end')
                Nothing ->
                    case body0 start env ps buf of
                        Nothing -> Nothing
                        Just (env'@(ntbs', _), start', end') ->
                                loop [access ntbs' nt1 x] start' end' end' env' ps buf
          where loop acc minStart maxEnd prev env ps buf =
                    case condition prev env ps buf of
                        Just ((ntbs', bs'), start', end') ->
                            Just ((ntbs', Map.insert "values" (SEQUENCE (reverse acc)) bs'),
                                  min start' minStart,
                                  max end' maxEnd)
                        Nothing ->
                            case body prev env ps buf of
                                Nothing -> Nothing
                                Just (env'@(ntbs', _), start', end') ->
                                    loop (access ntbs' nt1 x:acc)
                                         (min start' minStart)
                                         (max end' maxEnd)
                                         end'
                                         env'
                                         ps
                                         buf
                body0 = interpNonTerminal ctxt nt1 es1 l0 r0
                condition0 = interpNonTerminal ctxt nt2 es2 l0 r0
                body = interpNonTerminal ctxt nt1 es1 l r
                condition = interpNonTerminal ctxt nt2 es2 l r

access :: (HasCallStack) => NTBindings a -> (NT, Int) -> Id -> Value a
access env nt "this" = BINDINGS (this_ (env !!! nt))
access env nt "these" = SEQUENCE (map BINDINGS (these_ (env !!! nt)))
access env nt x = case this_ (env !!! nt) of bs -> bs !!! x

eval :: (HasCallStack) => Context a -> Int -> Environment a -> Parameters a -> Exp' -> Value a
eval ctxt eoi (env, bs) ps = go
    where go T = BOOL True
          go F = BOOL False
          go (Int n) = INT n
          go (Float f) = FLOAT f
          go (String s) = STRING s
          go (Bin Add l r) = add (go l) (go r)
              where add (INT x) (INT y) = INT (x + y)
                    add (INT x) (FLOAT y) = FLOAT (fromIntegral x + y)
                    add (FLOAT x) (INT y) = FLOAT (x + fromIntegral y)
                    add (FLOAT x) (FLOAT y) = FLOAT (x + y)
          go (Bin Sub l r) = sub (go l) (go r)
              where sub (INT x) (INT y) = INT (x - y)
                    sub (INT x) (FLOAT y) = FLOAT (fromIntegral x - y)
                    sub (FLOAT x) (INT y) = FLOAT (x - fromIntegral y)
                    sub (FLOAT x) (FLOAT y) = FLOAT (x - y)
          go (Bin Mul l r) = mul (go l) (go r)
              where mul (INT x) (INT y) = INT (x * y)
                    mul (INT x) (FLOAT y) = FLOAT (fromIntegral x * y)
                    mul (FLOAT x) (INT y) = FLOAT (x * fromIntegral y)
                    mul (FLOAT x) (FLOAT y) = FLOAT (x * y)
          go (Bin Div l r) = div' (go l) (go r)
              where div' (INT x) (INT y) = INT (x `quot` y)
                    div' (INT x) (FLOAT y) = FLOAT (fromIntegral x / y)
                    div' (FLOAT x) (INT y) = FLOAT (x / fromIntegral y)
                    div' (FLOAT x) (FLOAT y) = FLOAT (x / y)
          go (Bin Mod l r) = mod' (go l) (go r)
              where mod' (INT x) (INT y) = INT (x `rem` y)
          go (Bin Exp l r) = exp' (go l) (go r)
              where exp' (FLOAT x) (FLOAT y) = FLOAT (x ** y)
                    exp' (FLOAT x) (INT y) = FLOAT (x ^ y)
                    exp' (INT x) (FLOAT y) = FLOAT (fromIntegral x ** y)
                    exp' (INT x) (INT y) = INT (x ^ y)
          go (Un Neg l) = neg (go l)
              where neg (INT x) = INT (-x)
                    neg (FLOAT x) = FLOAT (-x)
          go (Un BitwiseNeg l) = bneg (go l)
              where bneg (INT x) = INT (complement x)
          go (Un Not l) = not' (go l)
              where not' (BOOL b) = BOOL (not b)
                    not' (INT n) = BOOL (n == 0)
          go (Bin And l r) = and' (go l) (go r)
              where and' (BOOL x) (BOOL y) = BOOL (x && y)
                    and' (BOOL x) (INT y) = BOOL (x && y /= 0)
                    and' (INT x) (BOOL y) = BOOL (x /= 0 && y)
                    and' (INT x) (INT y) = BOOL (x /= 0 && y /= 0)
          go (Bin Or l r) = or' (go l) (go r)
              where or' (BOOL x) (BOOL y) = BOOL (x || y)
                    or' (BOOL x) (INT y) = BOOL (x || y /= 0)
                    or' (INT x) (BOOL y) = BOOL (x /= 0 || y)
                    or' (INT x) (INT y) = BOOL (x /= 0 || y /= 0)
          go (Bin BitwiseAnd l r) = band (go l) (go r)
              where band (INT x) (INT y) = INT (x .&. y)
          go (Bin BitwiseXor l r) = bxor (go l) (go r)
              where bxor (INT x) (INT y) = INT (xor x y)
          go (Bin BitwiseOr l r) = bor (go l) (go r)
              where bor (INT x) (INT y) = INT (x .|. y)
          go (Bin LSh l r) = lsh (go l) (go r)
              where lsh (INT x) (INT y) = INT (shift x (fromIntegral y))
          go (Bin RSh l r) = rsh (go l) (go r)
              where rsh (INT x) (INT y) = INT (shift x (-fromIntegral y))
          go (Bin LessThan l r) = lessThan (go l) (go r)
              where lessThan (INT x) (INT y) = BOOL (x < y)
                    lessThan (INT x) (FLOAT y) = BOOL (fromIntegral x < y)
                    lessThan (FLOAT x) (INT y) = BOOL (x < fromIntegral y)
                    lessThan (FLOAT x) (FLOAT y) = BOOL (x < y)
                    lessThan (STRING x) (STRING y) = BOOL (x < y)
                    lessThan _ _ = BOOL False -- TODO: Type conversions
          go (Bin LTE l r) = lte (go l) (go r)
              where lte (INT x) (INT y) = BOOL (x <= y)
                    lte (INT x) (FLOAT y) = BOOL (fromIntegral x <= y)
                    lte (FLOAT x) (INT y) = BOOL (x <= fromIntegral y)
                    lte (FLOAT x) (FLOAT y) = BOOL (x <= y)
                    lte (STRING x) (STRING y) = BOOL (x <= y)
                    lte _ _ = BOOL False -- TODO: Type conversions
          go (Bin GreaterThan l r) = greaterThan (go l) (go r)
              where greaterThan (INT x) (INT y) = BOOL (x > y)
                    greaterThan (INT x) (FLOAT y) = BOOL (fromIntegral x > y)
                    greaterThan (FLOAT x) (INT y) = BOOL (x > fromIntegral y)
                    greaterThan (FLOAT x) (FLOAT y) = BOOL (x > y)
                    greaterThan (STRING x) (STRING y) = BOOL (x > y)
                    greaterThan _ _ = BOOL False -- TODO: Type conversions
          go (Bin GTE l r) = gte (go l) (go r)
              where gte (INT x) (INT y) = BOOL (x >= y)
                    gte (INT x) (FLOAT y) = BOOL (fromIntegral x >= y)
                    gte (FLOAT x) (INT y) = BOOL (x >= fromIntegral y)
                    gte (FLOAT x) (FLOAT y) = BOOL (x >= y)
                    gte (STRING x) (STRING y) = BOOL (x >= y)
                    gte _ _ = BOOL False -- TODO: Type conversions
          go (Bin Equal l r) = equal (go l) (go r)
              where equal (INT x) (INT y) = BOOL (x == y)
                    equal (INT x) (FLOAT y) = BOOL (fromIntegral x == y)
                    equal (FLOAT x) (INT y) = BOOL (x == fromIntegral y)
                    equal (FLOAT x) (FLOAT y) = BOOL (x == y)
                    equal (STRING x) (STRING y) = BOOL (x == y)
                    equal _ _ = BOOL False -- TODO: Type conversions
          go (Bin NotEqual l r) = notEqual (go l) (go r)
              where notEqual (INT x) (INT y) = BOOL (x /= y)
                    notEqual (INT x) (FLOAT y) = BOOL (fromIntegral x /= y)
                    notEqual (FLOAT x) (INT y) = BOOL (x /= fromIntegral y)
                    notEqual (FLOAT x) (FLOAT y) = BOOL (x /= y)
                    notEqual (STRING x) (STRING y) = BOOL (x /= y)
                    notEqual _ _ = BOOL False -- TODO: Type conversions
          go (If b t e) = if_ (go b) (go t) (go e)
              where if_ (INT x) y z = if x /= 0 then y else z
                    if_ (BOOL b') y z = if b' then y else z
          go (Call f es) = (externalFuncs ctxt !!! f) es'
            where es' = map go es
          go (Bin At l x) = at' (go l) (go x)
              where at' (SEQUENCE bs') (INT ix) = bs' !!. fromIntegral ix
                    at' (STRING bs') (INT ix) = INT (fromIntegral (BS.index bs' (fromIntegral ix)))
                    at' (BINDINGS bs') (STRING x') = bs' !!! x'
          go (Ref (Id x)) = case Map.lookup x ps of
                                Nothing -> case Map.lookup x bs of
                                                Nothing -> constants ctxt !!! x
                                                Just v -> v
                                Just v -> v
          go (Ref (Attr nt x)) = access env nt x
          go (Ref (Index nt e x)) = index (go e) (these_ ee)
              where ee = env !!! nt
                    offset = offset_ ee
                    index (INT ix) bs'
                        | x == "this" = BINDINGS (bs' !!. j)
                        | otherwise = case bs' !!. j of b -> b !!! x
                      where j = fromIntegral ix - offset
          go (Ref EOI) = INT (fromIntegral eoi)
          go (Ref (Start nt)) = INT (fromIntegral (start_ (env !!! nt)))
          go (Ref (End nt)) = INT (fromIntegral (end_ (env !!! nt)))
