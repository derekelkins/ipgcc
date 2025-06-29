{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Text.IPG.Check ( validate ) where
import qualified Data.Set as Set -- containers
import qualified Data.Map as Map -- containers

import Data.String.Interpolate ( i ) -- string-interpolate

import Text.IPG.Core ( Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..) )
import Text.IPG.GenericExp ( Exp(..) )
import Text.IPG.Parser ( IdType )

type T = IdType
type Exp' = Exp T T T
type Grammar' = Grammar T T T Exp' 
type Term' = Term T T T Exp'

-- Things to check:
--   - Referenced rules are defined or declared as external
--   - Attributes are defined before use
--   - EOI is not used as a parameter name
--   - _ipg_start and _ipg_end are not used as attribute names (maybe check this in Export.JS)
--   - _ipg_startsWith is not used as a rule name (maybe check this in Export.JS)
--   - START, END, this, these should not occur in the LHS of assignments 
--      (TODO: Be more discerning about `these`.)
--   - Rules invoked with the proper arities
validate :: Set.Set T -> Grammar' -> Maybe [T]
validate externalRules (Grammar rules) = mconcat (map check rules) <> basicChecks
  where
    possibleAttributes' =
        Map.fromList (map (\(Rule _ nt _ alts) -> (nt, Set.unions (map attrInAlt alts))) rules)
    -- TODO: "these" should only be added after an Array term
    specialAttrs = Set.fromList ["these", "this"]
    possibleAttributes = fmap (Set.union specialAttrs) possibleAttributes'
    parameters =
        Map.fromList (map (\(Rule _ nt args _) -> (nt, (length args, Set.fromList args))) rules)
    attrInAlt (Alternative terms) = Set.unions (map attrInTerm terms)
    attrInTerm (x := _) = Set.singleton x
    attrInTerm (Any x _) = Set.singleton x
    attrInTerm (Slice x _ _) = Set.singleton x
    attrInTerm (Repeat _ _ _ _ _ _ _) = Set.singleton "values"
    attrInTerm (RepeatUntil _ _ _ _ _ _ _ _ _) = Set.singleton "values"
    attrInTerm _ = Set.empty

    basicChecks = mconcat [
        if "_ipg_startsWith" `Set.member` externalRules
          || "_ipg_startsWith" `Map.member` parameters then
            Just ["_ipg_startsWith can't be the name of a Rule"] else Nothing,
        mconcat (map checkParameters (Map.toList parameters)),
        mconcat (map checkAttributes (Map.toList possibleAttributes'))
      ]
    checkParameters :: (T, (Int, Set.Set T)) -> Maybe [T]
    checkParameters (nt, (_, params))
        | "EOI" `Set.member` params = Just [[i|Rule #{nt} has illegal parameter name EOI|]]
        | otherwise = Nothing
    checkAttributes :: (T, Set.Set T) -> Maybe [T]
    checkAttributes (nt, attrs)
        | "START" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to START|]]
        | "END" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to END|]]
        | "this" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to this|]]
        | "these" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to these|]]
        | "_ipg_start" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to _ipg_start|]]
        | "_ipg_end" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to _ipg_end|]]
        | otherwise = Nothing

    check (Rule _ nt params alts) =
        mconcat (map (\(Alternative ts) -> checkTerms nt (Set.fromList params) Set.empty ts) alts)
    checkTerms :: T -> Set.Set T -> Set.Set T -> [Term'] -> Maybe [T]
    checkTerms _ _ _ [] = Nothing
    checkTerms nt params locals (NonTerminal a es l r:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> mconcat (map (checkExp nt params locals) es)
        <> checkExp nt params locals l
        <> checkExp nt params locals r
        <> checkTerms nt params locals ts
    checkTerms nt params locals (Terminal _ l r:ts) =
           checkExp nt params locals l
        <> checkExp nt params locals r
        <> checkTerms nt params locals ts
    checkTerms nt params locals ((x := e):ts) =
           checkExp nt params locals e
        <> checkTerms nt params locals' ts
      where locals' = Set.insert x locals
    checkTerms nt params locals (Any x e:ts) =
           checkExp nt params locals e
        <> checkTerms nt params locals' ts
      where locals' = Set.insert x locals
    checkTerms nt params locals (Slice x l r:ts) =
           checkExp nt params locals l
        <> checkExp nt params locals r
        <> checkTerms nt params locals' ts
      where locals' = Set.insert x locals
    checkTerms nt params locals (Guard e:ts) =
            checkExp nt params locals e
        <> checkTerms nt params locals ts
    checkTerms nt params locals (Repeat a es l r x l0 r0:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule {nt} is undefined|]]
            Just (n, _) -> if n == length es then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> (case Map.lookup a possibleAttributes of
                Nothing -> Nothing -- Already checked this case
                Just attrs -> if x `Set.member` attrs then Nothing
                                else Just [[i|#{x} isn't an attribute on #{a} in rule #{nt}|]])
        <> mconcat (map (checkExp nt params locals) es)
        <> checkExp nt params locals l
        <> checkExp nt params locals r
        <> checkExp nt params locals l0
        <> checkExp nt params locals r0
        <> checkTerms nt params locals' ts
      where locals' = Set.insert "values" locals
    checkTerms nt params locals (RepeatUntil a es1 l r x l0 r0 b es2:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es1 then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> (case Map.lookup a possibleAttributes of
                Nothing -> Nothing -- Already checked this case
                Just attrs -> if x `Set.member` attrs then Nothing
                                else Just [[i|#{x} isn't an attribute on #{a} in rule #{nt}|]])
        <> (case Map.lookup b parameters of
                Nothing -> if b `Set.member` externalRules then Nothing
                            else Just [[i|Rule #{b} in rule #{nt} is undefined|]]
                Just (n, _) -> if n == length es2 then Nothing
                                else Just [[i|Arity mismatch when calling #{b} in rule #{nt}|]])
        <> mconcat (map (checkExp nt params locals) es1)
        <> mconcat (map (checkExp nt params locals) es2)
        <> checkExp nt params locals l
        <> checkExp nt params locals r
        <> checkExp nt params locals l0
        <> checkExp nt params locals r0
        <> checkTerms nt params locals' ts
      where locals' = Set.insert "values" locals
    checkTerms nt params locals (Array x s e a es l r:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> checkExp nt params locals s
        <> checkExp nt params locals e
        <> mconcat (map (checkExp nt params' locals) es)
        <> checkExp nt params' locals l
        <> checkExp nt params' locals r
        <> checkTerms nt params locals ts
      where params' = Set.insert x params
    checkExp :: T -> Set.Set T -> Set.Set T -> Exp' -> Maybe [T]
    checkExp nt params locals (Add l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Sub l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Mul l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Div l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Mod l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Exp l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (And l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Or l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (BitwiseAnd l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (BitwiseXor l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (BitwiseOr l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (LSh l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (RSh l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (LessThan l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (LTE l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (GreaterThan l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (GTE l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Equal l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (NotEqual l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (At l r) =
        checkExp nt params locals l <> checkExp nt params locals r
    checkExp nt params locals (Neg l) = checkExp nt params locals l
    checkExp nt params locals (BitwiseNeg l) = checkExp nt params locals l
    checkExp nt params locals (Not l) = checkExp nt params locals l
    checkExp nt params locals (If b t e) =
        checkExp nt params locals b <> checkExp nt params locals t
            <> checkExp nt params locals e
    checkExp nt params locals (Call _ es) = mconcat (map (checkExp nt params locals) es)
    checkExp nt params locals (Ref (Id x)) =
        if x `Set.member` params || x `Set.member` locals then Nothing
            else Just [[i|#{x} is not yet defined in rule #{nt}|]]
    checkExp nt _ _ (Ref (Attr a x)) =
        case Map.lookup a possibleAttributes of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just attrs -> if x `Set.member` attrs then Nothing
                            else Just [[i|#{x} isn't an attribute on #{a} in rule #{nt}|]]
    checkExp nt params locals (Ref (Index a e x)) =
        (case Map.lookup a possibleAttributes of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just attrs -> if x `Set.member` attrs then Nothing
                            else Just [[i|#{x} isn't an attribute on #{a} in rule #{nt}|]])
        <> checkExp nt params locals e
    checkExp nt _ _ (Ref (Start a)) =
        if a `Map.member` parameters || a `Set.member` externalRules then Nothing
            else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
    checkExp nt _ _ (Ref (End a)) =
        if a `Map.member` parameters || a `Set.member` externalRules then Nothing
            else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
    checkExp _ _ _ _ = Nothing
