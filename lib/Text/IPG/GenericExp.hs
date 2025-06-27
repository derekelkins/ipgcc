module Text.IPG.GenericExp ( Exp(..), simplify, simplifyExp ) where
import Data.Bits ( shift, complement, xor, (.&.), (.|.) ) -- base

import Text.IPG.CoreIPG ( Grammar, Ref )

data Exp nt t id
    = Int Integer
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

simplify :: (Ord t) => Grammar nt t id (Exp nt t id) -> Grammar nt t id (Exp nt t id)
simplify = fmap simplifyExp

simplifyExp :: (Ord t) => Exp nt t id -> Exp nt t id
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
    where and' (Int x) y = if x /= 0 then y else Int 0
          and' x (Int y) = if y /= 0 then x else Int 0
          and' x y = And x y
simplifyExp (Or l r) = or' (simplifyExp l) (simplifyExp r) 
    where or' (Int x) y = if x == 0 then y else Int 1
          or' x (Int y) = if y == 0 then x else Int 1
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
    where lessThan (Int x) (Int y) = Int (if x < y then 1 else 0)
          lessThan (Float x) (Float y) = Int (if x < y then 1 else 0)
          lessThan (String x) (String y) = Int (if x < y then 1 else 0)
          lessThan x y = LessThan x y
simplifyExp (LTE l r) = lte (simplifyExp l) (simplifyExp r) 
    where lte (Int x) (Int y) = Int (if x <= y then 1 else 0)
          lte (Float x) (Float y) = Int (if x <= y then 1 else 0)
          lte (String x) (String y) = Int (if x <= y then 1 else 0) 
          lte x y = LTE x y
simplifyExp (GreaterThan l r) = greaterThan (simplifyExp l) (simplifyExp r) 
    where greaterThan (Int x) (Int y) = Int (if x > y then 1 else 0)
          greaterThan (Float x) (Float y) = Int (if x > y then 1 else 0)
          greaterThan (String x) (String y) = Int (if x > y then 1 else 0)
          greaterThan x y = GreaterThan x y
simplifyExp (GTE l r) = gte (simplifyExp l) (simplifyExp r) 
    where gte (Int x) (Int y) = Int (if x >= y then 1 else 0)
          gte (Float x) (Float y) = Int (if x >= y then 1 else 0)
          gte (String x) (String y) = Int (if x >= y then 1 else 0)
          gte x y = GTE x y
simplifyExp (Equal l r) = equal (simplifyExp l) (simplifyExp r) 
    where equal (Int x) (Int y) = Int (if x == y then 1 else 0)
          equal (Float x) (Float y) = Int (if x == y then 1 else 0)
          equal (String x) (String y) = Int (if x == y then 1 else 0)
          equal x y = Equal x y
simplifyExp (NotEqual l r) = notEqual (simplifyExp l) (simplifyExp r) 
    where notEqual (Int x) (Int y) = Int (if x /= y then 1 else 0)
          notEqual (Float x) (Float y) = Int (if x /= y then 1 else 0)
          notEqual (String x) (String y) = Int (if x /= y then 1 else 0)
          notEqual x y = NotEqual x y
simplifyExp (If b t e) = if_ (simplifyExp b) (simplifyExp t) (simplifyExp e)
    where if_ (Int x) y z = if x == 0 then z else y
          if_ x y z = If x y z
simplifyExp (Call t es) = Call t (map simplifyExp es)
simplifyExp (At e ix) = At (simplifyExp e) (simplifyExp ix)
simplifyExp (Ref r) = Ref (fmap simplifyExp r)
