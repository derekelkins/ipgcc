{-# LANGUAGE OverloadedStrings #-}
module CheckIPG ( validate ) where
import Control.Applicative ( asum, (<|>) ) -- base
import Data.String ( IsString ) -- base
import Text.Printf ( printf, PrintfArg ) -- base
import qualified Data.Set as Set -- containers
import qualified Data.Map as Map -- containers
import CoreIPG ( Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..) )
import GenericExp ( Exp(..) )

-- Things to check:
--   - Referenced rules are defined or declared as external
--   - Attributes are defined before use
--   - EOI is not used as a parameter name
--   - _ipg_start and _ipg_end are not used as attribute names (maybe check this in JSExport)
--   - _ipg_startsWith is not used as a rule name (maybe check this in JSExport)
--   - START, END, this, these should not occur in the LHS of assignments 
--      (TODO: Be more discerning about `these`.)
--   - Rules invoked with the proper arities
validate
    :: (PrintfArg nt, PrintfArg id, IsString id, IsString nt, Ord nt, Ord id)
    => Set.Set nt
    -> Grammar nt t id (Exp nt t id)
    -> Maybe [String]
validate externalRules (Grammar rules) = asum (map check rules) <|> basicChecks
  where
    possibleAttributes' =
        Map.fromList (map (\(Rule _ nt _ alts) -> (nt, Set.unions (map attrInAlt alts))) rules)
    -- TODO: "these" should only be added after an Array term
    specialAttrs = Set.fromList ["these", "this"]
    possibleAttributes = fmap (Set.union specialAttrs) possibleAttributes'
    parameters =
        Map.fromList (map (\(Rule _ nt args _) -> (nt, (length args, Set.fromList args))) rules)
    attrInAlt (Alternative terms) = Set.unions (map attrInTerm terms)
    attrInTerm (i := _) = Set.singleton i
    attrInTerm (Any i _) = Set.singleton i
    attrInTerm (Slice i _ _) = Set.singleton i
    attrInTerm (Repeat _ _ _) = Set.singleton "values"
    attrInTerm (RepeatUntil _ _ _ _ _) = Set.singleton "values"
    attrInTerm _ = Set.empty

    basicChecks = asum [
        if "_ipg_startsWith" `Set.member` externalRules
          || "_ipg_startsWith" `Map.member` parameters then
            Just ["_ipg_startsWith can't be the name of a Rule"] else Nothing,
        asum (map checkParameters (Map.toList parameters)),
        asum (map checkAttributes (Map.toList possibleAttributes'))
      ]
    checkParameters (nt, (_, params))
        | "EOI" `Set.member` params = Just [printf "Rule %s has illegal parameter name EOI" nt]
        | otherwise = Nothing
    checkAttributes (nt, attrs)
        | "START" `Set.member` attrs = Just [printf "Rule %s illegally assigns to START" nt]
        | "END" `Set.member` attrs = Just [printf "Rule %s illegally assigns to END" nt]
        | "this" `Set.member` attrs = Just [printf "Rule %s illegally assigns to this" nt]
        | "these" `Set.member` attrs = Just [printf "Rule %s illegally assigns to these" nt]
        | "_ipg_start" `Set.member` attrs = Just [printf "Rule %s illegally assigns to _ipg_start" nt]
        | "_ipg_end" `Set.member` attrs = Just [printf "Rule %s illegally assigns to _ipg_end" nt]
        | otherwise = Nothing

    check (Rule _ nt params alts) =
        asum (map (\(Alternative ts) -> checkTerms nt (Set.fromList params) Set.empty ts) alts)
    checkTerms _ _ _ [] = Nothing
    checkTerms nt params locals (NonTerminal a es l r:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [printf "Rule %s in rule %s is undefined" a nt]
            Just (n, _) -> if n == length es then Nothing
                            else Just [printf "Arity mismatch when calling %s in rule %s" a nt])
        <|> asum (map (checkExp nt params locals) es)
        <|> checkExp nt params locals l
        <|> checkExp nt params locals r
        <|> checkTerms nt params locals ts
    checkTerms nt params locals (Terminal _ l r:ts) =
            checkExp nt params locals l
        <|> checkExp nt params locals r
        <|> checkTerms nt params locals ts
    checkTerms nt params locals ((i := e):ts) =
            checkExp nt params locals e
        <|> checkTerms nt params locals' ts
      where locals' = Set.insert i locals
    checkTerms nt params locals (Any i e:ts) =
            checkExp nt params locals e
        <|> checkTerms nt params locals' ts
      where locals' = Set.insert i locals
    checkTerms nt params locals (Slice i l r:ts) =
            checkExp nt params locals l
        <|> checkExp nt params locals r
        <|> checkTerms nt params locals' ts
      where locals' = Set.insert i locals
    checkTerms nt params locals (Guard e:ts) =
            checkExp nt params locals e
        <|> checkTerms nt params locals ts
    checkTerms nt params locals (Repeat a es i:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [printf "Rule %s in rule %s is undefined" a nt]
            Just (n, _) -> if n == length es then Nothing
                            else Just [printf "Arity mismatch when calling %s in rule %s" a nt])
        <|> (case Map.lookup a possibleAttributes of
                Nothing -> Nothing -- Already checked this case
                Just attrs -> if i `Set.member` attrs then Nothing
                                else Just [printf "%s isn't an attribute on %s in rule %s" i a nt])
        <|> asum (map (checkExp nt params locals) es)
        <|> checkTerms nt params locals' ts
      where locals' = Set.insert "values" locals
    checkTerms nt params locals (RepeatUntil a es1 i b es2:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [printf "Rule %s in rule %s is undefined" a nt]
            Just (n, _) -> if n == length es1 then Nothing
                            else Just [printf "Arity mismatch when calling %s in rule %s" a nt])
        <|> (case Map.lookup a possibleAttributes of
                Nothing -> Nothing -- Already checked this case
                Just attrs -> if i `Set.member` attrs then Nothing
                                else Just [printf "%s isn't an attribute on %s in rule %s" i a nt])
        <|> (case Map.lookup b parameters of
                Nothing -> if b `Set.member` externalRules then Nothing
                            else Just [printf "Rule %s in rule %s is undefined" b nt]
                Just (n, _) -> if n == length es2 then Nothing
                                else Just [printf "Arity mismatch when calling %s in rule %s" b nt])
        <|> asum (map (checkExp nt params locals) es1)
        <|> asum (map (checkExp nt params locals) es2)
        <|> checkTerms nt params locals' ts
      where locals' = Set.insert "values" locals
    checkTerms nt params locals (Array i s e a es l r:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [printf "Rule %s in rule %s is undefined" a nt]
            Just (n, _) -> if n == length es then Nothing
                            else Just [printf "Arity mismatch when calling %s in rule %s" a nt])
        <|> checkExp nt params locals s
        <|> checkExp nt params locals e
        <|> asum (map (checkExp nt params' locals) es)
        <|> checkExp nt params' locals l
        <|> checkExp nt params' locals r
        <|> checkTerms nt params locals ts
      where params' = Set.insert i params
    checkExp nt params locals (Add l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Sub l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Mul l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Div l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Mod l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Exp l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (And l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Or l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (BitwiseAnd l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (BitwiseXor l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (BitwiseOr l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (LSh l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (RSh l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (LessThan l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (LTE l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (GreaterThan l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (GTE l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Equal l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (NotEqual l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (At l r) =
        checkExp nt params locals l <|> checkExp nt params locals r
    checkExp nt params locals (Neg l) = checkExp nt params locals l
    checkExp nt params locals (BitwiseNeg l) = checkExp nt params locals l
    checkExp nt params locals (Not l) = checkExp nt params locals l
    checkExp nt params locals (If b t e) =
        checkExp nt params locals b <|> checkExp nt params locals t
            <|> checkExp nt params locals e
    checkExp nt params locals (Call _ es) = asum (map (checkExp nt params locals) es)
    checkExp nt params locals (Ref (Id i)) =
        if i `Set.member` params || i `Set.member` locals then Nothing
            else Just [printf "%s is not yet defined in rule %s" i nt]
    checkExp nt _ _ (Ref (Attr a i)) =
        case Map.lookup a possibleAttributes of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [printf "Rule %s in rule %s is undefined" a nt]
            Just attrs -> if i `Set.member` attrs then Nothing
                            else Just [printf "%s isn't an attribute on %s in rule %s" i a nt]
    checkExp nt params locals (Ref (Index a e i)) =
        (case Map.lookup a possibleAttributes of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [printf "Rule %s in rule %s is undefined" a nt]
            Just attrs -> if i `Set.member` attrs then Nothing
                            else Just [printf "%s isn't an attribute on %s in rule %s" i a nt])
        <|> checkExp nt params locals e
    checkExp nt _ _ (Ref (Start a)) =
        if a `Map.member` parameters || a `Set.member` externalRules then Nothing
            else Just [printf "Rule %s in rule %s is undefined" a nt]
    checkExp nt _ _ (Ref (End a)) =
        if a `Map.member` parameters || a `Set.member` externalRules then Nothing
            else Just [printf "Rule %s in rule %s is undefined" a nt]
    checkExp _ _ _ _ = Nothing
