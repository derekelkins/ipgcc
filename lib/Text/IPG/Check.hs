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

u :: (T, Int) -> String
u (nt, -1) = [i|#{nt}|] -- Shouldn't happen as output of toCore.
u (nt, n) = [i|#{nt}@#{show n}|]

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
validate externalRules (Grammar rules) = foldMap check rules <> basicChecks
  where
    possibleAttributes' =
        Map.fromList (map (\(Rule _ nt _ alts) -> (nt, Set.unions (map attrInAlt alts))) rules)
    guaranteedAttributes' =
        Map.fromList (map (\(Rule _ nt _ alts) ->
                            (nt, foldr1 Set.intersection (map attrInAlt alts))) rules)
    -- TODO: "these" should only be added after an Array term
    specialAttrs = Set.fromList ["these", "this"]
    guaranteedAttributes = fmap (Set.union specialAttrs) guaranteedAttributes'
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
        foldMap checkParameters (Map.toList parameters),
        foldMap checkAttributes (Map.toList possibleAttributes')
      ]
    checkParameters :: (T, (Int, Set.Set T)) -> Maybe [T]
    checkParameters (nt, (_, params))
        | "EOI" `Set.member` params
            = Just [[i|Rule #{nt} has illegal parameter name EOI|]] -- This can't happen in parsed input
        | otherwise = Nothing
    checkAttributes :: (T, Set.Set T) -> Maybe [T]
    checkAttributes (nt, attrs)
            -- The START and END failures can't happen in the output of the parser.
        | "START" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to START|]]
        | "END" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to END|]]
        | "this" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to this|]]
        | "these" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to these|]]
        | "_ipg_start" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to _ipg_start|]]
        | "_ipg_end" `Set.member` attrs = Just [[i|Rule #{nt} illegally assigns to _ipg_end|]]
        | otherwise = Nothing

    check (Rule _ nt params alts) =
        foldMap (\(Alternative ts) ->
            checkTerms nt (Set.fromList params) Set.empty Set.empty ts) alts
    checkTerms :: T -> Set.Set T -> Set.Set T -> Set.Set (T, Int) -> [Term'] -> Maybe [T]
    checkTerms _ _ _ _ [] = Nothing
    checkTerms nt params locals nts (NonTerminal nt'@(a, _) es l r:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> foldMap (checkExp nt params locals nts) es
        <> checkExp nt params locals nts l
        <> checkExp nt params locals nts r
        <> checkTerms nt params locals nts' ts
      where nts' = Set.insert nt' nts
    checkTerms nt params locals nts (Terminal _ l r:ts) =
           checkExp nt params locals nts l
        <> checkExp nt params locals nts r
        <> checkTerms nt params locals nts ts
    checkTerms nt params locals nts ((x := e):ts) =
           checkExp nt params locals nts e
        <> checkTerms nt params locals' nts ts
      where locals' = Set.insert x locals
    checkTerms nt params locals nts (Any x e:ts) =
           checkExp nt params locals nts e
        <> checkTerms nt params locals' nts ts
      where locals' = Set.insert x locals
    checkTerms nt params locals nts (Slice x l r:ts) =
           checkExp nt params locals nts l
        <> checkExp nt params locals nts r
        <> checkTerms nt params locals' nts ts
      where locals' = Set.insert x locals
    checkTerms nt params locals nts (Guard e:ts) =
            checkExp nt params locals nts e
        <> checkTerms nt params locals nts ts
    checkTerms nt params locals nts (Repeat nt'@(a, _) es l r x l0 r0:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> (case Map.lookup a guaranteedAttributes of
                Nothing -> Nothing -- Already checked this case
                Just attrs -> if x `Set.member` attrs then Nothing
                                else Just [[i|#{x} isn't a guaranteed attribute on #{a} in rule #{nt}|]])
        <> foldMap (checkExp nt params locals nts') es
        <> checkExp nt params locals nts' l
        <> checkExp nt params locals nts' r
        <> checkExp nt params locals nts l0
        <> checkExp nt params locals nts r0
        <> checkTerms nt params locals' nts' ts
      where locals' = Set.insert "values" locals
            nts' = Set.insert nt' nts
    checkTerms nt params locals nts (RepeatUntil nt1'@(a, _) es1 l r x l0 r0 nt2'@(b, _) es2:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es1 then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> (case Map.lookup a guaranteedAttributes of
                Nothing -> Nothing -- Already checked this case
                Just attrs -> if x `Set.member` attrs then Nothing
                                else Just [[i|#{x} isn't a guaranteed attribute on #{a} in rule #{nt}|]])
        <> (case Map.lookup b parameters of
                Nothing -> if b `Set.member` externalRules then Nothing
                            else Just [[i|Rule #{b} in rule #{nt} is undefined|]]
                Just (n, _) -> if n == length es2 then Nothing
                                else Just [[i|Arity mismatch when calling #{b} in rule #{nt}|]])
        <> foldMap (checkExp nt params locals nts') es1
        <> foldMap (checkExp nt params locals nts') es2
        <> checkExp nt params locals nts' l
        <> checkExp nt params locals nts' r
        <> checkExp nt params locals nts l0
        <> checkExp nt params locals nts r0
        <> checkTerms nt params locals' nts'' ts
      where locals' = Set.insert "values" locals
            nts' = Set.insert nt1' nts
            nts'' = Set.insert nt2' nts'
    checkTerms nt params locals nts (Array x s e nt'@(a, _) es l r:ts) =
        (case Map.lookup a parameters of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just (n, _) -> if n == length es then Nothing
                            else Just [[i|Arity mismatch when calling #{a} in rule #{nt}|]])
        <> checkExp nt params locals nts s
        <> checkExp nt params locals nts e
        <> foldMap (checkExp nt params' locals nts') es
        <> checkExp nt params' locals nts' l
        <> checkExp nt params' locals nts' r
        <> checkTerms nt params locals nts' ts
      where params' = Set.insert x params
            nts' = Set.insert nt' nts
    checkExp :: T -> Set.Set T -> Set.Set T -> Set.Set (T, Int) -> Exp' -> Maybe [T]
    checkExp nt params locals nts (Bin _ l r) =
        checkExp nt params locals nts l <> checkExp nt params locals nts r
    checkExp nt params locals nts (Un _ l) = checkExp nt params locals nts l
    checkExp nt params locals nts (If b t e) =
        checkExp nt params locals nts b <> checkExp nt params locals nts t
            <> checkExp nt params locals nts e
    checkExp nt params locals nts (Call _ es) = foldMap (checkExp nt params locals nts) es
    checkExp nt params locals _ (Ref (Id x)) =
        if x `Set.member` params || x `Set.member` locals then Nothing
            else Just [[i|#{x} is not yet defined in rule #{nt}|]] -- This isn't possible as the output of toCore.
    checkExp nt _ _ nts (Ref (Attr nt'@(a, _) x)) =
        (case Map.lookup a guaranteedAttributes of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just attrs -> if x `Set.member` attrs then Nothing
                            else Just [[i|#{x} isn't a guaranteed attribute on #{a} in rule #{nt}|]])
        <> if nt' `Set.member` nts  then Nothing
               else Just [[i|Alias #{u nt'} in rule #{nt} is undefined|]]
    checkExp nt params locals nts (Ref (Index nt'@(a, _) e x)) =
        (case Map.lookup a guaranteedAttributes of
            Nothing -> if a `Set.member` externalRules then Nothing
                        else Just [[i|Rule #{a} in rule #{nt} is undefined|]]
            Just attrs -> if x `Set.member` attrs then Nothing
                            else Just [[i|#{x} isn't a guaranteed attribute on #{a} in rule #{nt}|]])
        <> checkExp nt params locals nts e
        <> if nt' `Set.member` nts  then Nothing
               else Just [[i|Alias #{u nt'} in rule #{nt} is undefined|]]
    checkExp nt _ _ nts (Ref (Start nt'@(a, _))) =
        (if a `Map.member` parameters || a `Set.member` externalRules then Nothing
            else Just [[i|Rule #{a} in rule #{nt} is undefined|]])
        <> if nt' `Set.member` nts  then Nothing
                else Just [[i|Alias #{u nt'} in rule #{nt} is undefined|]]
    checkExp nt _ _ nts (Ref (End nt'@(a, _))) =
        (if a `Map.member` parameters || a `Set.member` externalRules then Nothing
            else Just [[i|Rule #{a} in rule #{nt} is undefined|]])
        <> if nt' `Set.member` nts  then Nothing
                else Just [[i|Alias #{u nt'} in rule #{nt} is undefined|]]
    checkExp _ _ _ _ _ = Nothing
