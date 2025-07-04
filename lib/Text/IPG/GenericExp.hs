module Text.IPG.GenericExp (
    UnOp(..), BinOp(..), Exp(..), crushRef, mapRef, simplify, simplifyExp
) where
import Data.Bits ( shift, complement, xor, (.&.), (.|.) ) -- base
import Data.Int ( Int64 ) -- base

import Text.IPG.Core ( Grammar, Ref )

data UnOp = Not | Neg | BitwiseNeg deriving ( Eq, Ord, Show )

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Exp
    | And
    | Or
    | BitwiseAnd
    | BitwiseXor
    | BitwiseOr
    | LSh
    | RSh
    | LessThan
    | LTE
    | GreaterThan
    | GTE
    | Equal
    | NotEqual
    | At
  deriving ( Eq, Ord, Show )

data Exp nt t id
    = T
    | F
    | Int Int64
    | Float Double
    | String t
    | Un UnOp (Exp nt t id)
    | Bin BinOp (Exp nt t id) (Exp nt t id)
    | If (Exp nt t id) (Exp nt t id) (Exp nt t id)
    | Call t [Exp nt t id]
    | Ref (Ref nt id (Exp nt t id))
  deriving ( Show )

mapRef :: (Ref nt id (Exp nt t id) -> Ref nt' id (Exp nt' t id)) -> Exp nt t id -> Exp nt' t id
mapRef _ T = T
mapRef _ F = F
mapRef f (Un op l) = Un op (mapRef f l)
mapRef f (Bin op l r) = Bin op (mapRef f l) (mapRef f r)
mapRef f (If b t e) = If (mapRef f b) (mapRef f t) (mapRef f e)
mapRef f (Call t es) = Call t (map (mapRef f) es)
mapRef f (Ref r) = Ref (f r)
mapRef _ (Int n) = Int n
mapRef _ (Float n) = Float n
mapRef _ (String s) = String s

crushRef :: (Monoid m) => (Ref nt id (Exp nt t id) -> m) -> Exp nt t id -> m
crushRef _ T = mempty
crushRef _ F = mempty
crushRef f (Un _ l) = crushRef f l
crushRef f (Bin _ l r) = crushRef f l <> crushRef f r
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
simplifyExp (Bin Add l r) = add (simplifyExp l) (simplifyExp r)
    where add (Int 0) y = y
          add x (Int 0) = x
          add (Int x) (Int y) = Int (x + y)
          add (Float 0) y = y
          add x (Float 0) = x
          add (Bin Add x (Int y)) (Int z) = add x (Int (y + z))
          add (Int x) (Bin Add (Int y) z) = add (Int (x + y)) z
          add (Bin Sub x (Int y)) (Int z) = add x (Int (z - y))
          add (Float x) (Float y) = Float (x + y)
          add x y = Bin Add x y
simplifyExp (Bin Sub l r) = sub (simplifyExp l) (simplifyExp r)
    where sub (Int 0) y = simplifyExp (Un Neg y)
          sub x (Int 0) = x
          sub (Int x) (Int y) = Int (x - y)
          sub (Float 0) y = simplifyExp (Un Neg y)
          sub x (Float 0) = x
          sub (Float x) (Float y) = Float (x - y)
          sub x y = Bin Sub x y
simplifyExp (Bin Mul l r) = mul (simplifyExp l) (simplifyExp r)
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
          mul x y = Bin Mul x y
simplifyExp (Bin Div l r) = div' (simplifyExp l) (simplifyExp r)
    where div' (Float x) (Float y) = Float (x / y)
          div' x (Int 1) = x
          div' x (Float 1) = x
          div' x y = Bin Div x y
simplifyExp (Bin Mod l r) = mod' (simplifyExp l) (simplifyExp r)
    where mod' (Int x) (Int y) = Int (x `rem` y)
          mod' x y = Bin Mod x y
simplifyExp (Bin Exp l r) = exp' (simplifyExp l) (simplifyExp r)
    where exp' (Float x) (Float y) = Float (x ** y)
          exp' (Int x) (Int y) = Int (x ^ y)
          exp' _ (Int 0) = Int 1
          exp' x (Int 1) = x
          exp' x y = Bin Exp x y
simplifyExp (Un Neg l) = neg (simplifyExp l)
    where neg (Int x) = Int (-x)
          neg (Float x) = Float (-x)
          neg (Un Neg x) = x
          neg x = Un Neg x
simplifyExp (Un BitwiseNeg l) = bneg (simplifyExp l)
    where bneg (Int x) = Int (complement x)
          bneg (Un BitwiseNeg x) = x
          bneg x = Un BitwiseNeg x
simplifyExp (Un Not l) = Un Not (simplifyExp l)
simplifyExp (Bin And l r) = and' (simplifyExp l) (simplifyExp r)
    where and' (Int x) y = if x /= 0 then y else F
          and' x (Int y) = if y /= 0 then x else F
          and' T y = y
          and' F _ = F
          and' x T = x
          and' _ F = F
          and' x y = Bin And x y
simplifyExp (Bin Or l r) = or' (simplifyExp l) (simplifyExp r)
    where or' (Int x) y = if x == 0 then y else T
          or' x (Int y) = if y == 0 then x else T
          or' F y = y
          or' T _ = T
          or' x F = x
          or' _ T = T
          or' x y = Bin Or x y
simplifyExp (Bin BitwiseAnd l r) = band (simplifyExp l) (simplifyExp r)
    where band (Int x) (Int y) = Int (x .&. y)
          band x y = Bin BitwiseAnd x y
simplifyExp (Bin BitwiseXor l r) = bxor (simplifyExp l) (simplifyExp r)
    where bxor (Int x) (Int y) = Int (xor x y)
          bxor x y = Bin BitwiseXor x y
simplifyExp (Bin BitwiseOr l r) = bor (simplifyExp l) (simplifyExp r)
    where bor (Int x) (Int y) = Int (x .|. y)
          bor x y = Bin BitwiseOr x y
simplifyExp (Bin LSh l r) = lsh (simplifyExp l) (simplifyExp r)
    where lsh (Int x) (Int y) = Int (shift x (fromIntegral y))
          lsh x y = Bin LSh x y
simplifyExp (Bin RSh l r) = rsh (simplifyExp l) (simplifyExp r)
    where rsh (Int x) (Int y) = Int (shift x (-fromIntegral y))
          rsh x y = Bin RSh x y
simplifyExp (Bin LessThan l r) = lessThan (simplifyExp l) (simplifyExp r)
    where lessThan (Int x) (Int y) = if x < y then T else F
          lessThan (Float x) (Float y) = if x < y then T else F
          lessThan (String x) (String y) = if x < y then T else F
          lessThan x y = Bin LessThan x y
simplifyExp (Bin LTE l r) = lte (simplifyExp l) (simplifyExp r)
    where lte (Int x) (Int y) = if x <= y then T else F
          lte (Float x) (Float y) = if x <= y then T else F
          lte (String x) (String y) = if x <= y then T else F
          lte x y = Bin LTE x y
simplifyExp (Bin GreaterThan l r) = greaterThan (simplifyExp l) (simplifyExp r)
    where greaterThan (Int x) (Int y) = if x > y then T else F
          greaterThan (Float x) (Float y) = if x > y then T else F
          greaterThan (String x) (String y) = if x > y then T else F
          greaterThan x y = Bin GreaterThan x y
simplifyExp (Bin GTE l r) = gte (simplifyExp l) (simplifyExp r)
    where gte (Int x) (Int y) = if x >= y then T else F
          gte (Float x) (Float y) = if x >= y then T else F
          gte (String x) (String y) = if x >= y then T else F
          gte x y = Bin GTE x y
simplifyExp (Bin Equal l r) = equal (simplifyExp l) (simplifyExp r)
    where equal (Int x) (Int y) = if x == y then T else F
          equal (Float x) (Float y) = if x == y then T else F
          equal (String x) (String y) = if x == y then T else F
          equal x y = Bin Equal x y
simplifyExp (Bin NotEqual l r) = notEqual (simplifyExp l) (simplifyExp r)
    where notEqual (Int x) (Int y) = if x /= y then T else F
          notEqual (Float x) (Float y) = if x /= y then T else F
          notEqual (String x) (String y) = if x /= y then T else F
          notEqual x y = Bin NotEqual x y
simplifyExp (If b t e) = if_ (simplifyExp b) (simplifyExp t) (simplifyExp e)
    where if_ (Int x) y z = if x == 0 then z else y
          if_ T y _ = y
          if_ F _ z = z
          if_ x y z = If x y z
simplifyExp (Call t es) = Call t (map simplifyExp es)
simplifyExp (Bin At e ix) = Bin At (simplifyExp e) (simplifyExp ix)
simplifyExp (Ref r) = Ref (fmap simplifyExp r)
