module GenericExp ( Exp(..) ) where
import CoreIPG

data Exp t
    = Int Integer
    | Float Double
    | String t
    | Add (Exp t) (Exp t)
    | Sub (Exp t) (Exp t)
    | Mul (Exp t) (Exp t)
    | Div (Exp t) (Exp t)
    | Neg (Exp t)
    | And (Exp t) (Exp t)
    | Or (Exp t) (Exp t)
    | LSh (Exp t) (Exp t)
    | RSh (Exp t) (Exp t)
    | LessThan (Exp t) (Exp t)
    | LTE (Exp t) (Exp t)
    | GreaterThan (Exp t) (Exp t)
    | GTE (Exp t) (Exp t)
    | Equal (Exp t) (Exp t)
    | If (Exp t) (Exp t) (Exp t)
    | Call t [Exp t]
    | At (Exp t) (Exp t)
    | Ref (Ref t t (Exp t))
  deriving ( Show )
