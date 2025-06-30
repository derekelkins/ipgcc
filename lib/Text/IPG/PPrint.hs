{-# LANGUAGE OverloadedStrings #-}
module Text.IPG.PPrint (
    pprint, pprintRule, pprintExpr, pprintAlternative, pprintTerm, pprintRef, pprintMetaTag,
    pprint', pprintRule', pprintAlternative', pprintTerm', pprintRef', pprintNT,
    floatToOut, hexyString, outParen
) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import Data.Char ( ord ) -- base
import Data.List ( intersperse ) -- base

import Text.IPG.Core ( Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..), MetaTag(..) )
import Text.IPG.GenericExp ( Exp(..) )

type T = BS.ByteString
type Out = Builder.Builder

outParen :: Bool -> Out -> Out
outParen True x = "(" <> x <> ")"
outParen False x = x

hexyString :: T -> Out
hexyString s = "\"" <> foldMap go (BS.unpack s) <> "\""
    where go 0x5C = "\\\\"
          go 0x22 = "\\\""
          go c | isPrintable c = Builder.word8 c
               | otherwise = "\\x" <> paddedHex c
          paddedHex c = if c >= 16 then Builder.word8Hex c else "0" <> Builder.word8Hex c
          isPrintable c = c >= 0x20 && c <= 0x7E

floatToOut :: Double -> Out
floatToOut = foldMap (Builder.word8 . fromIntegral . ord) . show -- TODO: Crude

pprintMetaTag :: MetaTag -> Out
pprintMetaTag INSTRUMENT = "%instrument"

pprintNT :: (T, Int) -> Out
pprintNT (nt, -1) = Builder.byteString nt -- This case shouldn't really happen.
pprintNT (nt, n) = Builder.byteString nt <> "@" <> Builder.intDec n

pprint :: Grammar T T T (Exp T T T) -> Out
pprint = pprint' (pprintExpr 0)

pprint' :: (e -> Out) -> Grammar T T T e -> Out
pprint' ppExp (Grammar rules) = mconcat (intersperse "\n\n" (map (pprintRule' ppExp) rules))

pprintRule :: Rule T T T (Exp T T T) -> Out
pprintRule = pprintRule' (pprintExpr 0)

pprintRule' :: (e -> Out) -> Rule T T T e -> Out
pprintRule' ppExp (Rule mts nt [] alts) =
    foldMap ((<> " ") . pprintMetaTag) mts  <> Builder.byteString nt <> "\n  -> "
 <> mconcat (intersperse "\n   / " (map (pprintAlternative' ppExp) alts)) <> ";"
pprintRule' ppExp (Rule mts nt args alts) =
    foldMap ((<> " ") . pprintMetaTag) mts
 <> Builder.byteString nt <> "("
 <> mconcat (intersperse ", " (map Builder.byteString args))
 <> ")\n  -> " <> mconcat (intersperse "\n   / " (map (pprintAlternative' ppExp) alts)) <> ";"

pprintAlternative :: Alternative T T T (Exp T T T) -> Out
pprintAlternative = pprintAlternative' (pprintExpr 0)

pprintAlternative' :: (e -> Out) -> Alternative T T T e -> Out
pprintAlternative' ppExp (Alternative terms) =
    mconcat (intersperse "\n     " (map (pprintTerm' ppExp) terms))

pprintTerm :: Term T T T (Exp T T T) -> Out
pprintTerm = pprintTerm' (pprintExpr 0)

pprintArgList :: (e -> Out) -> [e] -> Out
pprintArgList _ [] = ""
pprintArgList ppExp es = "(" <> mconcat (intersperse ", " (map ppExp es)) <> ")"

pprintInterval :: (e -> Out) -> e -> e -> Out
pprintInterval ppExp l r = "[" <> ppExp l <> ", " <> ppExp r <> "]"

pprintTerm' :: (e -> Out) -> Term T T T e -> Out
pprintTerm' ppExp (NonTerminal nt es l r) =
    pprintNT nt <> pprintArgList ppExp es <> pprintInterval ppExp l r
pprintTerm' ppExp (Terminal t l r) = hexyString t <> pprintInterval ppExp l r
pprintTerm' ppExp (x := e) = "{ " <> Builder.byteString x <> " = " <> ppExp e <> " }"
pprintTerm' ppExp (Guard e) = "?[ " <> ppExp e <> " ]"
pprintTerm' ppExp (Array j s e nt es l r) =
    "for " <> Builder.byteString j <> " = " <> ppExp s <> " to " <> ppExp e <> " do "
 <> pprintNT nt <> pprintArgList ppExp es <> pprintInterval ppExp l r
pprintTerm' ppExp (Any x e) = "{ " <> Builder.byteString x <> " = .[" <> ppExp e <> "] }"
pprintTerm' ppExp (Slice x l r) =
    "{ " <> Builder.byteString x <> " = *" <> pprintInterval ppExp l r <> " }"
pprintTerm' ppExp (Repeat nt es l r x l0 r0) =
    "repeat " <> pprintNT nt <> pprintArgList ppExp es <> pprintInterval ppExp l r
 <> "." <> Builder.byteString x <> " starting on " <> pprintInterval ppExp l0 r0
pprintTerm' ppExp (RepeatUntil nt1 es1 l r x l0 r0 nt2 es2) =
    "repeat " <> pprintNT nt1 <> pprintArgList ppExp es1 <> pprintInterval ppExp l r
 <> "." <> Builder.byteString x <> " starting on " <> pprintInterval ppExp l0 r0
 <> " until " <> pprintNT nt2 <> pprintArgList ppExp es2

pprintRef :: Ref T T (Exp T T T) -> Out
pprintRef = pprintRef' (pprintExpr 0)

pprintRef' :: (e -> Out) -> Ref T T e -> Out
pprintRef' _ (Id x) = Builder.byteString x
pprintRef' _ (Attr nt x) = pprintNT nt <> "." <> Builder.byteString x
pprintRef' ppExp (Index nt e x) =
    pprintNT nt <> "(" <> ppExp e <> ")." <> Builder.byteString x
pprintRef' _ EOI = "EOI"
pprintRef' _ (Start nt) = pprintNT nt <> ".START"
pprintRef' _ (End nt) = pprintNT nt <> ".END"

pprintExpr :: Int -> Exp T T T -> Out
pprintExpr _ (Int n) = Builder.integerDec n
pprintExpr _ (Float n) = floatToOut n
pprintExpr _ (String s) = hexyString s
pprintExpr p (Add l r) =
    outParen (p > 11) (pprintExpr 11 l <> " + " <> pprintExpr 12 r)
pprintExpr p (Sub l r) =
    outParen (p > 11) (pprintExpr 11 l <> " - " <> pprintExpr 12 r)
pprintExpr p (Mul l r) =
    outParen (p > 12) (pprintExpr 12 l <> " * " <> pprintExpr 13 r)
pprintExpr p (Div l r) =
    outParen (p > 12) (pprintExpr 12 l <> " / " <> pprintExpr 13 r)
pprintExpr p (Mod l r) =
    outParen (p > 12) (pprintExpr 12 l <> " % " <> pprintExpr 13 r)
pprintExpr p (Exp l r) =
    outParen (p > 13) (pprintExpr 14 l <> " ** " <> pprintExpr 13 r)
pprintExpr p (Neg e) =
    outParen (p > 14) ("-" <> pprintExpr 15 e)
pprintExpr p (BitwiseNeg e) =
    outParen (p > 14) ("~" <> pprintExpr 15 e)
pprintExpr p (And l r) =
    outParen (p > 4) (pprintExpr 4 l <> " && " <> pprintExpr 5 r)
pprintExpr p (Or l r) =
    outParen (p > 3)  (pprintExpr 3 l <> " || " <> pprintExpr 4 r)
pprintExpr p (BitwiseAnd l r) =
    outParen (p > 7) (pprintExpr 7 l <> " & " <> pprintExpr 8 r)
pprintExpr p (BitwiseXor l r) =
    outParen (p > 6)  (pprintExpr 6 l <> " ^ " <> pprintExpr 7 r)
pprintExpr p (BitwiseOr l r) =
    outParen (p > 5)  (pprintExpr 5 l <> " | " <> pprintExpr 6 r)
pprintExpr p (LSh l r) =
    outParen (p > 10) (pprintExpr 10 l <> " << " <> pprintExpr 11 r)
pprintExpr p (RSh l r) =
    outParen (p > 10) (pprintExpr 10 l <> " >> " <> pprintExpr 11 r)
pprintExpr p (LessThan l r) =
    outParen (p > 9) (pprintExpr 9 l <> " < " <> pprintExpr 10 r)
pprintExpr p (LTE l r) =
    outParen (p > 9) (pprintExpr 9 l <> " <= " <> pprintExpr 10 r)
pprintExpr p (GreaterThan l r) =
    outParen (p > 9) (pprintExpr 9 l <> " > " <> pprintExpr 10 r)
pprintExpr p (GTE l r) =
    outParen (p > 9) (pprintExpr 9 l <> " >= " <> pprintExpr 10 r)
pprintExpr p (Equal l r) =
    outParen (p > 8) (pprintExpr 8 l <> " == " <> pprintExpr 9 r)
pprintExpr p (NotEqual l r) =
    outParen (p > 8) (pprintExpr 8 l <> " != " <> pprintExpr 9 r)
pprintExpr p (Not l) =
    outParen (p > 14) ("!" <> pprintExpr 15 l)
pprintExpr p (If b t e) =
    outParen (p > 2)
        (pprintExpr 2 b <> " ? " <> pprintExpr 3 t <> " : " <> pprintExpr 3 e)
pprintExpr _ (Call t es) =
    Builder.byteString t <> "(" <> mconcat (intersperse ", " $ map (pprintExpr 0) es) <> ")"
pprintExpr p (At l r) =
    outParen (p > 17) (pprintExpr 17 l <> "[" <> pprintExpr 0 r <> "]")
pprintExpr _ (Ref r) = pprintRef r
