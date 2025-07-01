module Text.IPG.GenericExp ( Exp(..), crushRef, mapRef, simplify, simplifyExp ) where
import Data.Bits ( shift, complement, xor, (.&.), (.|.) ) -- base
import Data.Int ( Int64 ) -- base

import Text.IPG.Core ( Grammar, Ref )

data Exp nt t id
    = T
    | F
    | Int Int64
    | Float Double
    | String t
    | Add (Exp nt t id) (Exp nt t id)
    | Sub (Exp nt t id) (Exp nt t id)
    | Mul (Exp nt t id) (Exp nt t id)
    | Div (Exp nt t id) (Exp nt t id)
    | Mod (Exp nt t id) (Exp nt t id)
    | Exp (Exp nt t id) (Exp nt t id)
    | Neg (Exp nt t id)
    | BitwiseNeg (Exp nt t id)
    | And (Exp nt t id) (Exp nt t id)
    | Or (Exp nt t id) (Exp nt t id)
    | BitwiseAnd (Exp nt t id) (Exp nt t id)
    | BitwiseXor (Exp nt t id) (Exp nt t id)
    | BitwiseOr (Exp nt t id) (Exp nt t id)
    | LSh (Exp nt t id) (Exp nt t id)
    | RSh (Exp nt t id) (Exp nt t id)
    | LessThan (Exp nt t id) (Exp nt t id)
    | LTE (Exp nt t id) (Exp nt t id)
    | GreaterThan (Exp nt t id) (Exp nt t id)
    | GTE (Exp nt t id) (Exp nt t id)
    | Equal (Exp nt t id) (Exp nt t id)
    | NotEqual (Exp nt t id) (Exp nt t id)
    | Not (Exp nt t id)
    | If (Exp nt t id) (Exp nt t id) (Exp nt t id)
    | Call t [Exp nt t id]
    | At (Exp nt t id) (Exp nt t id)
    | Ref (Ref nt id (Exp nt t id))
  deriving ( Show )

mapRef :: (Ref nt id (Exp nt t id) -> Ref nt' id (Exp nt' t id)) -> Exp nt t id -> Exp nt' t id
mapRef _ T = T
mapRef _ F = F
mapRef f (Not l) = Not (mapRef f l)
mapRef f (Neg l) = Neg (mapRef f l)
mapRef f (BitwiseNeg l) = BitwiseNeg (mapRef f l)
mapRef f (Add l r) = Add (mapRef f l) (mapRef f r)
mapRef f (Sub l r) = Sub (mapRef f l) (mapRef f r)
mapRef f (Mul l r) = Mul (mapRef f l) (mapRef f r)
mapRef f (Div l r) = Div (mapRef f l) (mapRef f r)
mapRef f (Mod l r) = Mod (mapRef f l) (mapRef f r)
mapRef f (Exp l r) = Exp (mapRef f l) (mapRef f r)
mapRef f (And l r) = And (mapRef f l) (mapRef f r)
mapRef f (Or l r) = Or (mapRef f l) (mapRef f r)
mapRef f (BitwiseAnd l r) = BitwiseAnd (mapRef f l) (mapRef f r)
mapRef f (BitwiseXor l r) = BitwiseXor (mapRef f l) (mapRef f r)
mapRef f (BitwiseOr l r) = BitwiseOr (mapRef f l) (mapRef f r)
mapRef f (LSh l r) = LSh (mapRef f l) (mapRef f r)
mapRef f (RSh l r) = RSh (mapRef f l) (mapRef f r)
mapRef f (LessThan l r) = LessThan (mapRef f l) (mapRef f r)
mapRef f (LTE l r) = LTE (mapRef f l) (mapRef f r)
mapRef f (GreaterThan l r) = GreaterThan (mapRef f l) (mapRef f r)
mapRef f (GTE l r) = GTE (mapRef f l) (mapRef f r)
mapRef f (Equal l r) = Equal (mapRef f l) (mapRef f r)
mapRef f (NotEqual l r) = NotEqual (mapRef f l) (mapRef f r)
mapRef f (At l r) = At (mapRef f l) (mapRef f r)
mapRef f (If b t e) = If (mapRef f b) (mapRef f t) (mapRef f e)
mapRef f (Call t es) = Call t (map (mapRef f) es)
mapRef f (Ref r) = Ref (f r)
mapRef _ (Int n) = Int n
mapRef _ (Float n) = Float n
mapRef _ (String s) = String s

crushRef :: (Monoid m) => (Ref nt id (Exp nt t id) -> m) -> Exp nt t id -> m
crushRef _ T = mempty
crushRef _ F = mempty
crushRef f (Not l) = crushRef f l
crushRef f (Neg l) = crushRef f l
crushRef f (BitwiseNeg l) = crushRef f l
crushRef f (Add l r) = crushRef f l <> crushRef f r
crushRef f (Sub l r) = crushRef f l <> crushRef f r
crushRef f (Mul l r) = crushRef f l <> crushRef f r
crushRef f (Div l r) = crushRef f l <> crushRef f r
crushRef f (Mod l r) = crushRef f l <> crushRef f r
crushRef f (Exp l r) = crushRef f l <> crushRef f r
crushRef f (And l r) = crushRef f l <> crushRef f r
crushRef f (Or l r) = crushRef f l <> crushRef f r
crushRef f (BitwiseAnd l r) = crushRef f l <> crushRef f r
crushRef f (BitwiseXor l r) = crushRef f l <> crushRef f r
crushRef f (BitwiseOr l r) = crushRef f l <> crushRef f r
crushRef f (LSh l r) = crushRef f l <> crushRef f r
crushRef f (RSh l r) = crushRef f l <> crushRef f r
crushRef f (LessThan l r) = crushRef f l <> crushRef f r
crushRef f (LTE l r) = crushRef f l <> crushRef f r
crushRef f (GreaterThan l r) = crushRef f l <> crushRef f r
crushRef f (GTE l r) = crushRef f l <> crushRef f r
crushRef f (Equal l r) = crushRef f l <> crushRef f r
crushRef f (NotEqual l r) = crushRef f l <> crushRef f r
crushRef f (At l r) = crushRef f l <> crushRef f r
crushRef f (If b t e) = crushRef f b <> crushRef f t <> crushRef f e
crushRef f (Call _ es) = foldMap (crushRef f) es
crushRef f (Ref r) = f r
crushRef _ _ = mempty

simplify :: (Ord t) => Grammar nt t id (Exp nt t id) -> Grammar nt t id (Exp nt t id)
simplify = fmap simplifyExp

simplifyExp :: (Ord t) => Exp nt t id -> Exp nt t id
simplifyExp T = T
simplifyExp F = F
simplifyExp (Int n) = Int n
simplifyExp (Float n) = Float n
simplifyExp (String s) = String s
simplifyExp (Add l r) = add (simplifyExp l) (simplifyExp r)
    where add (Int 0) y = y
          add x (Int 0) = x
          add (Int x) (Int y) = Int (x + y)
          add (Float 0) y = y
          add x (Float 0) = x
          add (Add x (Int y)) (Int z) = add x (Int (y + z))
          add (Int x) (Add (Int y) z) = add (Int (x + y)) z
          add (Sub x (Int y)) (Int z) = add x (Int (z - y))
          add (Float x) (Float y) = Float (x + y)
          add x y = Add x y
simplifyExp (Sub l r) = sub (simplifyExp l) (simplifyExp r)
    where sub (Int 0) y = simplifyExp (Neg y)
          sub x (Int 0) = x
          sub (Int x) (Int y) = Int (x - y)
          sub (Float 0) y = simplifyExp (Neg y)
          sub x (Float 0) = x
          sub (Float x) (Float y) = Float (x - y)
          sub x y = Sub x y
simplifyExp (Mul l r) = mul (simplifyExp l) (simplifyExp r)
    where mul (Int 0) _ = Int 0 -- TODO: Check types.
          mul _ (Int 0) = Int 0
          mul x (Int 1) = x
          mul (Int 1) y = y
          mul (Int x) (Int y) = Int (x * y)
          mul (Float 0) _ = Float 0
          mul _ (Float 0) = Float 0
          mul (Float 1) y = y
          mul x (Float 1) = x
          mul (Float x) (Float y) = Float (x * y)
          mul x y = Mul x y
simplifyExp (Div l r) = div' (simplifyExp l) (simplifyExp r)
    where div' (Float x) (Float y) = Float (x / y)
          div' x (Float 1) = x
          div' x y = Div x y
simplifyExp (Mod l r) = mod' (simplifyExp l) (simplifyExp r)
    where mod' (Int x) (Int y) = Int (x `rem` y)
          mod' x y = Mod x y
simplifyExp (Exp l r) = exp' (simplifyExp l) (simplifyExp r)
    where exp' (Float x) (Float y) = Float (x ** y)
          exp' (Int x) (Int y) = Int (x ^ y)
          exp' _ (Int 0) = Int 1
          exp' x (Int 1) = x
          exp' x y = Exp x y
simplifyExp (Neg l) = neg (simplifyExp l)
    where neg (Int x) = Int (-x)
          neg (Float x) = Float (-x)
          neg (Neg x) = x
          neg x = Neg x
simplifyExp (BitwiseNeg l) = bneg (simplifyExp l)
    where bneg (Int x) = Int (complement x)
          bneg (BitwiseNeg x) = x
          bneg x = BitwiseNeg x
simplifyExp (Not l) = Not (simplifyExp l)
simplifyExp (And l r) = and' (simplifyExp l) (simplifyExp r)
    where and' (Int x) y = if x /= 0 then y else F
          and' x (Int y) = if y /= 0 then x else F
          and' T y = y
          and' F _ = F
          and' x T = x
          and' _ F = F
          and' x y = And x y
simplifyExp (Or l r) = or' (simplifyExp l) (simplifyExp r)
    where or' (Int x) y = if x == 0 then y else T
          or' x (Int y) = if y == 0 then x else T
          or' F y = y
          or' T _ = T
          or' x F = x
          or' _ T = T
          or' x y = Or x y
simplifyExp (BitwiseAnd l r) = band (simplifyExp l) (simplifyExp r)
    where band (Int x) (Int y) = Int (x .&. y)
          band x y = BitwiseAnd x y
simplifyExp (BitwiseXor l r) = bxor (simplifyExp l) (simplifyExp r)
    where bxor (Int x) (Int y) = Int (xor x y)
          bxor x y = BitwiseXor x y
simplifyExp (BitwiseOr l r) = bor (simplifyExp l) (simplifyExp r)
    where bor (Int x) (Int y) = Int (x .|. y)
          bor x y = BitwiseOr x y
simplifyExp (LSh l r) = lsh (simplifyExp l) (simplifyExp r)
    where lsh (Int x) (Int y) = Int (shift x (fromIntegral y))
          lsh x y = LSh x y
simplifyExp (RSh l r) = rsh (simplifyExp l) (simplifyExp r)
    where rsh (Int x) (Int y) = Int (shift x (-fromIntegral y))
          rsh x y = RSh x y
simplifyExp (LessThan l r) = lessThan (simplifyExp l) (simplifyExp r)
    where lessThan (Int x) (Int y) = if x < y then T else F
          lessThan (Float x) (Float y) = if x < y then T else F
          lessThan (String x) (String y) = if x < y then T else F
          lessThan x y = LessThan x y
simplifyExp (LTE l r) = lte (simplifyExp l) (simplifyExp r)
    where lte (Int x) (Int y) = if x <= y then T else F
          lte (Float x) (Float y) = if x <= y then T else F
          lte (String x) (String y) = if x <= y then T else F
          lte x y = LTE x y
simplifyExp (GreaterThan l r) = greaterThan (simplifyExp l) (simplifyExp r)
    where greaterThan (Int x) (Int y) = if x > y then T else F
          greaterThan (Float x) (Float y) = if x > y then T else F
          greaterThan (String x) (String y) = if x > y then T else F
          greaterThan x y = GreaterThan x y
simplifyExp (GTE l r) = gte (simplifyExp l) (simplifyExp r)
    where gte (Int x) (Int y) = if x >= y then T else F
          gte (Float x) (Float y) = if x >= y then T else F
          gte (String x) (String y) = if x >= y then T else F
          gte x y = GTE x y
simplifyExp (Equal l r) = equal (simplifyExp l) (simplifyExp r)
    where equal (Int x) (Int y) = if x == y then T else F
          equal (Float x) (Float y) = if x == y then T else F
          equal (String x) (String y) = if x == y then T else F
          equal x y = Equal x y
simplifyExp (NotEqual l r) = notEqual (simplifyExp l) (simplifyExp r)
    where notEqual (Int x) (Int y) = if x /= y then T else F
          notEqual (Float x) (Float y) = if x /= y then T else F
          notEqual (String x) (String y) = if x /= y then T else F
          notEqual x y = NotEqual x y
simplifyExp (If b t e) = if_ (simplifyExp b) (simplifyExp t) (simplifyExp e)
    where if_ (Int x) y z = if x == 0 then z else y
          if_ T y _ = y
          if_ F _ z = z
          if_ x y z = If x y z
simplifyExp (Call t es) = Call t (map simplifyExp es)
simplifyExp (At e ix) = At (simplifyExp e) (simplifyExp ix)
simplifyExp (Ref r) = Ref (fmap simplifyExp r)
