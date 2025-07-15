{-# LANGUAGE OverloadedStrings #-}
module Text.IPG.TypeCheck (
    Context(..), ConstTypes, FunTypes, RuleTypes, TypeDefs, TypeDefs', Bindings, Bindings',
    Environments(typeDefs, constTypes, funTypes, ruleTypes),
    annotate, isEquatable, isOrderable, joinTy, subTypeOf, (<:), typeCheck, getRows,
) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as CLBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import Data.List ( intersperse ) -- base
import qualified Data.Map as Map -- containers
import Data.Maybe ( catMaybes ) -- base
import qualified Data.Set as Set -- containers

import GHC.Stack ( HasCallStack ) -- base

import Text.IPG.Core (
    Ty, Ty'(..), Grammar(..), Declaration(..), Rule(..), Alternative(..), Term(..), Ref(..),
    mapTyVar, partitionDeclarations, trimapTerm )
import Text.IPG.GenericExp ( BinOp(..), Exp(..), UnOp(..), trimap )
import Text.IPG.PPrint ( Out, pprintExpr, pprintTerm, pprintType' )

-- Bidirectional

-- Simple row types with associated subtyping rule.

-- Probably HM or so.

-- Type "synonyms", but allow recursive type synonyms as declared equi-recursive types.
-- However, these must be guarded by a record constructors.

-- ```ipg
-- typedef Rec(a) = { value: a, children: [Rec(a)] };
-- rule Foo(x: Int): { y: Int, z: Bool, root: Rec(Int) };
-- ```

-- TODO: Improve error messages. This will probably require me to push through
-- lexer positions.

type Note nt t id = nt

outToString :: Out -> String
outToString = CLBS.unpack . Builder.toLazyByteString

toS :: Out -> BS.ByteString
toS = BS.toStrict . Builder.toLazyByteString

ppList :: (a -> Out) -> [a] -> Out
ppList f = mconcat . intersperse ", " . map f

ppType :: Context nt t id -> Ty id nt -> Out
ppType ctxt = pprintType' (out ctxt) . fmap (toS . ntOut ctxt)

ppExp :: Context nt t id -> Exp nt t id nt -> Out
ppExp ctxt =
    pprintExpr 0
    . trimap (toS . ntOut ctxt) (toS . tOut ctxt) (toS . out ctxt)
    . fmap (toS . ntOut ctxt)

ppTerm :: Context nt t id -> Term nt t id (Exp nt t id nt) -> Out
ppTerm ctxt =
    pprintTerm
    . trimapTerm (toS . ntOut ctxt) (toS . tOut ctxt) (toS . out ctxt)
        (trimap (toS . ntOut ctxt) (toS . tOut ctxt) (toS . out ctxt))
    . fmap (fmap (toS . ntOut ctxt))

-- TODO: Consider these.
isEquatable :: (Show id, Ord id, HasCallStack) => Environments nt t id -> Ty id nt -> Bool
isEquatable _ (ArrayTy _) = False
isEquatable _ (RowTy _) = False
isEquatable _ (ExternalTy _) = False -- TODO: Do I want to allow this?
isEquatable envs (Note _ ty) = isEquatable envs ty
isEquatable envs (TyApp t ts) = isEquatable envs (tyApp (typeDefs envs) t ts)
isEquatable _ _ = True

isOrderable :: (Show id, Ord id, HasCallStack) => Environments nt t id -> Ty id nt -> Bool
isOrderable _ (ArrayTy _) = False
isOrderable _ (RowTy _) = False
isOrderable _ (ExternalTy _) = False
isOrderable envs (Note _ ty) = isOrderable envs ty
isOrderable envs (TyApp t ts) = isOrderable envs (tyApp (typeDefs envs) t ts)
isOrderable _ _ = True

aOut :: (nt -> Out) -> (nt, Int) -> Out
aOut ntOut' (nt, i) = ntOut' nt <> "@" <> Builder.intDec i

(<:) :: (Show id, Ord id) => Ty id x -> Ty id x -> Bool
ty1 <: ty2 = subTypeOf Map.empty ty1 ty2

subTypeOf :: (Show id, Ord id, HasCallStack) => TypeDefs x id -> Ty id x -> Ty id x -> Bool
subTypeOf tyEnv ty ty' = fst (subTypeOf' tyEnv True Map.empty ty ty')

type Bindings' x v id = Map.Map v (Ty' v id x)
type Bindings x id = Bindings' x id id

subTypeOf'
    :: (Show id, Ord id, Ord u, Ord v, HasCallStack)
    => TypeDefs' x u id
    -> Bool
    -> Bindings' x v id
    -> Ty' v id x
    -> Ty' v id x
    -> (Bool, Bindings' x v id)
subTypeOf' tyEnv rigid bs0 t1 t2 = go bs0 (derefTy tyEnv t1) (derefTy tyEnv t2)
    where go bs BoolTy BoolTy = (True, bs)
          go bs U8Ty U8Ty = (True, bs)
          go bs U8Ty IntTy = (True, bs)
          go bs U8Ty FloatTy = (True, bs)
          go bs IntTy IntTy = (True, bs)
          go bs IntTy FloatTy = (True, bs)
          go bs FloatTy FloatTy = (True, bs)
          go bs StringTy StringTy = (True, bs)
          go bs (TyVar v) (TyVar v') | v == v' = (True, bs)
          go bs ty (TyVar v) | rigid = (False, bs)
                             | otherwise = (True, Map.insert v ty bs)
          go bs (TyVar v) ty = (True, Map.insert v ty bs)
          go bs (ExternalTy x) (ExternalTy y) = (x == y, bs)
          go bs (ArrayTy t) (ArrayTy t') = go bs (deref bs t) (deref bs t')
          go bs (RowTy fs) (RowTy fs') = process bs (Map.toList fs')
            where process bs' ((x', t'):rest) =
                    case Map.lookup x' fs of
                        Nothing -> (False, bs')
                        Just t ->
                            case go bs' (deref bs' t) (deref bs' t') of
                                (True, bs'') -> process bs'' rest
                                r -> r
                  process bs' [] = (True, bs')
          go bs _ _ = (False, bs)
          deref _ (TyApp t ts) = tyApp tyEnv t ts
          deref bs (Note _ ty) = deref bs ty
          deref bs ty@(TyVar v) =
            case Map.lookup v bs of
                Nothing -> ty
                Just ty' -> deref bs ty'
          deref _ ty = ty

-- Expand typedefs until we get a row type.
getRows :: (Show id, Ord id) => TypeDefs x id -> Ty id x -> Maybe (Map.Map id (Ty id x))
getRows tyEnv (TyApp t ts) = getRows tyEnv (tyApp tyEnv t ts)
getRows tyEnv (Note _ ty) = getRows tyEnv ty
getRows _ (RowTy fs) = Just fs
getRows _ _ = Nothing

groundType :: (Ord v, HasCallStack) => Bindings' x v id -> Ty' v id x -> Ty' v id x
groundType bs = mapTyVar f
    where f v =
            case Map.lookup v bs of
                Nothing -> TyVar v
                Just ty@(TyVar v') | v == v' -> ty
                Just ty -> groundType bs ty

typeCheckArgs
    :: (Show id, Ord id, HasCallStack)
    => Context nt t id
    -> TypeDefs nt id
    -> [Ty id nt]
    -> [Ty id nt]
    -> Either (Out, Int) (Bindings nt id)
typeCheckArgs ctxt tyEnv lTys rTys = go 0 Map.empty lTys' rTys'
  where lTys' = map (mapTyVar (TyVar . Left)) lTys
        rTys' = map (mapTyVar (TyVar . Right)) rTys
        restore = mapTyVar (TyVar . either id id)
        f _ (Left _, _) = Nothing
        f bs (Right v, ty) = Just (v, restore (groundType bs ty))
        go _ _ [] (_:_) = error "Arity mismatch in typeCheckArgs"
        go _ _ (_:_) [] = error "Arity mismatch in typeCheckArgs"
        go _ bs [] [] = Right (Map.fromList (catMaybes (map (f bs) (Map.toList bs))))
        go n bs (ty:tys) (ty':tys') =
            case subTypeOf' tyEnv False bs ty ty' of
                (False, _) -> Left (ppType ctxt (restore ty) <> " is not a subtype of "
                                 <> ppType ctxt (restore ty'), n)
                (True, bs') -> go (n+1) bs' tys tys'

applyBindings :: (Ord id, HasCallStack) => Bindings x id -> Ty id x -> Ty id x
applyBindings bs = mapTyVar go
    where go v = case Map.lookup v bs of Nothing -> TyVar v; Just ty -> ty

-- TODO: Handle type variables.
joinTy :: (Show id, Ord id, HasCallStack) => TypeDefs x id -> Ty id x -> Ty id x -> Maybe (Ty id x)
joinTy _ ty@(TyApp x []) (TyApp y []) | x == y = Just ty
joinTy tyEnv t1 t2 = go (derefTy tyEnv t1) (derefTy tyEnv t2)
    where go (RowTy fs1) (RowTy fs2) =
              case traverse (\k -> (,) k <$> joinTy tyEnv (fs1 Map.! k) (fs2 Map.! k))
                            (Set.toList commonFields) of
                  Nothing -> Nothing
                  Just fs -> Just (RowTy (Map.fromList fs))
            where commonFields = Set.intersection (Map.keysSet fs1) (Map.keysSet fs2)
          go (ArrayTy ty1) (ArrayTy ty2) =
              case joinTy tyEnv ty1 ty2 of
                  Nothing -> Nothing
                  Just ty -> Just (ArrayTy ty)
          go ty1 ty2 | subTypeOf tyEnv ty1 ty2 = Just ty2
                     | subTypeOf tyEnv ty2 ty1 = Just ty1
                     | otherwise = Nothing

derefTy
    :: (Show id, Ord id, Ord u, HasCallStack)
    => TypeDefs' x u id
    -> Ty' v id x
    -> Ty' v id x
derefTy tyEnv (TyApp t ts) = derefTy tyEnv (tyApp tyEnv t ts)
derefTy tyEnv (Note _ ty) = derefTy tyEnv ty
derefTy _ ty = ty

tyApp
    :: (Show id, Ord id, Ord u, HasCallStack)
    => TypeDefs' x u id
    -> id
    -> [Ty' v id x]
    -> Ty' v id x
tyApp tyEnv t ts =
    case Map.lookup t tyEnv of
        Just (vs, ty) -> -- TODO: Check that length vs == length ts
            let bindings = Map.fromList (zip vs ts)
            in mapTyVar (f bindings) ty
        Nothing -> error ("Unknown typedef name: " <> show t)
  where f bs v =
            case Map.lookup v bs of
                Just ty -> ty
                Nothing -> error "tyApp: Extra type variables"

type TypeDefs' x v id = Map.Map id ([v], Ty' v id x)
type TypeDefs x id = TypeDefs' x id id
type ConstTypes x id = Map.Map id (Ty id x)
type FunTypes x t id = Map.Map t ([Ty id x], Ty id x)
type RuleTypes x nt id = Map.Map nt ([Ty id x], Ty id x)

data Environments nt t id = Environments {
    typeDefs :: TypeDefs (Note nt t id) id,
    constTypes :: ConstTypes (Note nt t id) id,
    funTypes :: FunTypes (Note nt t id) t id,
    ruleTypes :: RuleTypes (Note nt t id) nt id,
    locals :: Map.Map id (Ty id (Note nt t id)),
    localRuleTypes :: Map.Map (nt, Int) (Ty id (Note nt t id))
  }

data Context nt t id = Context {
    currentRule :: nt,
    values :: id,
    out :: id -> Out,
    tOut :: t -> Out,
    ntOut :: nt -> Out
  }

annotate
    :: (Ord id, Ord nt, HasCallStack)
    => Environments nt t id
    -> Grammar (Note nt t id) nt t id e
    -> Grammar (Note nt t id) nt t id e
annotate envs (Grammar decls) = Grammar (go Set.empty decls)
  where go seen (ConstDeclaration n Nothing e:ds) = ConstDeclaration n (Just ty) e:go seen ds
          where ty = case Map.lookup n (constTypes envs) of Just y -> y
        go seen (RuleDeclaration nt _ _:ds) | nt `Set.member` seen = go seen ds
        go seen (d@(RuleDeclaration nt _ (Just _)):ds) = d:go (Set.insert nt seen) ds
        go seen (RuleDeclaration nt es Nothing:ds) =
            RuleDeclaration nt es (Just ty):go (Set.insert nt seen) ds
          where ty = case Map.lookup nt (ruleTypes envs) of Just (_, y) -> y
        go seen (d@(RuleDef (Rule _ nt _ _)):ds)
            | nt `Set.member` seen = d:go seen ds
            | otherwise =
                case Map.lookup nt (ruleTypes envs) of
                    Just ([], ty) -> RuleDeclaration nt [] (Just ty):d:go (Set.insert nt seen) ds
                    _ -> d:go seen ds
        go seen (d:ds) = d:go seen ds
        go _ [] = []

typeCheck
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Grammar (Note nt t id) nt t id (Exp nt t id (Note nt t id))
    -> Either Out (Environments nt t id)
typeCheck ctxt (Grammar decls) = case errs of [] -> Right envs; (err:_) -> Left err
  where (rules, consts, tyEnv, ruleDecls, funDecls) = partitionDeclarations decls
        -- TODO: Need to handle mutual recursion and args.
        tyEnv' = Map.fromList (map (\(n, vs, ty) -> (n, (vs, ty))) tyEnv)
        fTypes' = Map.fromList (map (\(n, tys, ty) -> (n, (map snd tys, ty))) funDecls)
        -- Add zero-arity rules that don't have explicit declarations so we don't need to write them.
        zeroArityRules = catMaybes
                            (map (\(Rule _ nt es _) -> if null es then Just nt else Nothing) rules)
        argTys' = Map.fromList (map (\(nt, es, mty) -> (nt, (map snd es, mty))) ruleDecls)
        argTys = Map.union argTys' (Map.fromList (map (\nt -> (nt, ([], Nothing))) zeroArityRules))
        (cErrs, cTypes) = typeCheckConsts ctxt envs [] Map.empty consts
        (errs, rTypes) = typeCheckRules ctxt envs argTys cErrs Map.empty rules
        envs = Environments {
                 typeDefs = tyEnv',
                 constTypes = cTypes,
                 funTypes = fTypes',
                 ruleTypes = rTypes,
                 locals = Map.empty,
                 localRuleTypes = Map.empty
               }

typeCheckConsts
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> [Out]
    -> ConstTypes (Note nt t id) id
    -> [(id, Maybe (Ty id (Note nt t id)), Exp nt t id (Note nt t id))]
    -> ([Out], ConstTypes (Note nt t id) id)
typeCheckConsts _ _ eAcc acc [] = (eAcc, acc)
typeCheckConsts ctxt envs eAcc acc (c@(n, _, _):cs) =
    typeCheckConsts ctxt envs eAcc' (Map.insert n ty acc) cs
  where (eAcc', ty) =
            case typeCheckConst ctxt envs c of
                Left err -> (err:eAcc, error ("TODO: typeCheckConsts\n" <> outToString err))
                Right ty' -> (eAcc, ty')

typeCheckConst
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> (id, Maybe (Ty id (Note nt t id)), Exp nt t id (Note nt t id))
    -> Either Out (Ty id (Note nt t id))
typeCheckConst ctxt envs (n, Nothing, e) =
    case typeSynthExp ctxt envs e of
        Right ty -> Right ty
        Left err -> Left ("Failed to type check const " <> out ctxt n <> ".\nError: " <> err)
typeCheckConst ctxt envs (n, Just ty, e) =
    case typeCheckExp ctxt envs e ty of
        Nothing -> Right ty
        Just err -> Left ("Failed to type check const " <> out ctxt n <> ".\nError: " <> err)

typeCheckRules
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> Map.Map nt ([Ty id (Note nt t id)], Maybe (Ty id (Note nt t id)))
    -> [Out]
    -> RuleTypes (Note nt t id) nt id
    -> [Rule nt t id (Exp nt t id (Note nt t id))]
    -> ([Out], RuleTypes (Note nt t id) nt id)
typeCheckRules _ _ _ eAcc acc [] = (eAcc, acc)
typeCheckRules ctxt envs argTys eAcc acc (r@(Rule _ nt _ _):rs) =
    if not (nt `Map.member` argTys) then (missingDeclErr:eAcc, error "TODO: typeCheckRules1")
        else typeCheckRules ctxt envs argTys eAcc' (Map.insert nt (tys, ty) acc) rs
  where (tys, mty) =
            case Map.lookup nt argTys of ~(Just y) -> y
        missingDeclErr = ntOut ctxt nt <> " needs a rule declaration because it has parameters"
        (eAcc', ty) =
            case typeCheckRule ctxt envs r tys mty of
                Left err -> (err:eAcc, error ("TODO: typeCheckRules2\n" <> outToString err))
                Right ty' -> (eAcc, ty')

typeCheckRule
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> Rule nt t id (Exp nt t id (Note nt t id))
    -> [Ty id (Note nt t id)]
    -> Maybe (Ty id (Note nt t id))
    -> Either Out (Ty id (Note nt t id))
typeCheckRule ctxt envs (Rule _ nt ps alts) tys ty =
    let envs' = envs { locals = Map.fromList (zip ps tys) } -- TODO: Put these in a separate Map?
    in case traverse (\(Alternative ts) -> typeCheckAlternative ctxt' envs' ts ty) alts of
        Left err -> Left err
        Right [] -> error "typeCheckRule: No alts. Should never happen."
        Right (ty':tys') -> combine ty' tys'
  where ctxt' = ctxt { currentRule = nt }
        join = joinTy (typeDefs envs)
        combine ty' [] = Right ty'
        combine ty1' (ty2':tys') =
            case join ty1' ty2' of
                Just ty' -> combine ty' tys'
                Nothing -> Left (ppType ctxt ty1' <> "\n\nfailed to join with\n\n"
                              <> ppType ctxt ty2')

typeCheckAlternative
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> [Term nt t id (Exp nt t id (Note nt t id))]
    -> Maybe (Ty id (Note nt t id))
    -> Either Out (Ty id (Note nt t id))
typeCheckAlternative ctxt envs ts Nothing =
    case typeSynthTerms ctxt envs ts of
        Left err -> Left err
        Right ty -> Right ty
typeCheckAlternative ctxt envs ts (Just ty') =
    case typeSynthTerms ctxt envs ts of
        Left err -> Left err
        Right ty | subTypeOf (typeDefs envs) ty ty' -> Right ty'
        Right ty -> Left (ppType ctxt ty <> " does not match expected type "
                       <> ppType ctxt ty' <> " in rule "
                       <> ntOut ctxt (currentRule ctxt) <> ".")

typeSynthTerms
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> [Term nt t id (Exp nt t id (Note nt t id))]
    -> Either Out (Ty id (Note nt t id))
typeSynthTerms ctxt envs = go Map.empty (localRuleTypes envs)
  where go rows _ [] = Right (RowTy rows)
        go rows ntbs (t:ts) =
            let envs' = envs {
                            -- TODO: Currently, this causes local fields to shadow parameters.
                            locals = Map.union rows (locals envs),
                            localRuleTypes = ntbs
                        }
            in case typeSynthTerm ctxt envs' rows t of
                Left err -> Left (err <>
                      "\n  In term: " <> ppTerm ctxt t <>
                      "\n  In rule: " <> ntOut ctxt (currentRule ctxt))
                Right (rows', ntbs') -> go rows' (Map.union ntbs' ntbs) ts

typeSynthTerm
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> Map.Map id (Ty id (Note nt t id))
    -> Term nt t id (Exp nt t id (Note nt t id))
    -> Either Out (Map.Map id (Ty id (Note nt t id)), Map.Map (nt, Int) (Ty id (Note nt t id)))
typeSynthTerm ctxt envs rows (Terminal _ l r) =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left (lerr <> "\n" <> rerr)
        (Just lerr, _) -> Left lerr
        (_, Just rerr) -> Left rerr
        (Nothing, Nothing) -> Right (rows, Map.empty)
typeSynthTerm ctxt envs rows (x := e) =
    case typeSynthExp ctxt envs e of
        Left err -> Left err
        Right ty -> Right (Map.insert x ty rows, Map.empty)
typeSynthTerm ctxt envs rows (Guard e) =
    case typeCheckExp ctxt envs e BoolTy of
        Just err -> Left err
        Nothing -> Right (rows, Map.empty)
typeSynthTerm ctxt envs rows (Any x e) =
    case typeCheckExp ctxt envs e IntTy of
        Just err -> Left err
        Nothing -> Right (Map.insert x U8Ty rows, Map.empty)
typeSynthTerm ctxt envs rows (Slice x l r) =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left (lerr <> "\n" <> rerr)
        (Just lerr, _) -> Left lerr
        (_, Just rerr) -> Left rerr
        (Nothing, Nothing) -> Right (Map.insert x StringTy rows, Map.empty)
typeSynthTerm ctxt envs rows (NonTerminal a@(nt, _) es l r) =
    (\(ty, _) -> (rows, Map.singleton a ty)) <$> typeCheckRuleInvoke ctxt envs nt es l r
typeSynthTerm ctxt envs rows (Array j s e a@(nt, _) es l r) =
    case (typeCheckExp ctxt envs s IntTy, typeCheckExp ctxt envs e IntTy) of
        (Just lerr, Just rerr) -> Left (lerr <> "\n" <> rerr)
        (Just lerr, _) -> Left lerr
        (_, Just rerr) -> Left rerr
        (Nothing, Nothing) -> -- TODO: Probably need to do something for `these` here.
            let envs' = envs { locals = Map.insert j IntTy (locals envs) }
            in (\(ty, _) -> (rows, Map.singleton a ty))
                <$> typeCheckRuleInvoke ctxt envs' nt es l r
typeSynthTerm ctxt envs rows (Repeat a@(nt, _) es l r x l0 r0) =
    case (typeCheckExp ctxt envs l0 IntTy, typeCheckExp ctxt envs r0 IntTy) of
        (Just lerr, Just rerr) -> Left (lerr <> "\n" <> rerr)
        (Just lerr, _) -> Left lerr
        (_, Just rerr) -> Left rerr
        (Nothing, Nothing) ->
            case typeCheckRuleInvoke ctxt envs nt es l r of
                Left err -> Left err
                Right (ty', fs)
                    | x' == "this" ->
                        Right (Map.insert (values ctxt) (ArrayTy (Note nt ty')) rows,
                               Map.singleton a ty')
                    | otherwise ->
                        case Map.lookup x fs of
                            Nothing -> Left (out ctxt x <> " is not a field on " <> ntOut ctxt nt)
                            Just ty -> Right (Map.insert (values ctxt) (ArrayTy ty) rows,
                                              Map.singleton a ty')
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthTerm ctxt envs rows (RepeatUntil a1@(nt1, _) es1 l r x l0 r0 a2@(nt2, _) es2) =
    case typeCheckRuleInvoke ctxt envs nt2 es2 l0 r0 of
        Left err -> Left err
        Right (ty2', _) ->
            case typeCheckRuleInvoke ctxt envs nt1 es1 l r of
                Left err -> Left err
                Right (ty1', fs)
                    | x' == "this" ->
                        Right (Map.insert (values ctxt) (ArrayTy (Note nt1 ty1')) rows,
                               Map.fromList [(a2, ty2'), (a1, ty1')])
                    | otherwise ->
                        case Map.lookup x fs of
                            Nothing -> Left (out ctxt x <> " is not a field on " <> ntOut ctxt nt1)
                            Just ty -> Right (Map.insert (values ctxt) (ArrayTy ty) rows,
                                              Map.fromList [(a2, ty2'), (a1, ty1')])
  where x' = Builder.toLazyByteString (out ctxt x)

typeCheckRuleInvoke
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> nt
    -> [Exp nt t id (Note nt t id)]
    -> Exp nt t id (Note nt t id)
    -> Exp nt t id (Note nt t id)
    -> Either Out (Ty id (Note nt t id), Map.Map id (Ty id (Note nt t id)))
typeCheckRuleInvoke ctxt envs nt es l r =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left (lerr <> "\n" <> rerr)
        (Just lerr, _) -> Left lerr
        (_, Just rerr) -> Left rerr
        (Nothing, Nothing) ->
            case traverse (typeSynthExp ctxt envs) es of
                Left err -> Left err
                Right tys ->
                    case Map.lookup nt (ruleTypes envs) of
                        Nothing -> Left ("Unknown rule " <> ntOut ctxt nt)
                        Just (tys', ty') ->
                            case typeCheckArgs ctxt (typeDefs envs) tys tys' of
                                Left (err, ix) -> Left (
                                    "Type mismatch in arguments when invoking " <> ntOut ctxt nt
                                 <> ".\nArgument: " <> ppExp ctxt (es !! ix)
                                 -- <> "\nExpected argument types: " <> ppList (ppType ctxt) tys'
                                 -- <> "\nActual argument types: " <> ppList (ppType ctxt) tys
                                 <> "\nError: " <> err)
                                Right bs ->
                                    let ty'' = applyBindings bs ty'
                                    in Right (ty'', fromRowTy (derefTy (typeDefs envs) ty''))
  where fromRowTy ~(RowTy fs) = fs

typeCheckExp
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id (Note nt t id)
    -> Ty id (Note nt t id)
    -> Maybe Out
typeCheckExp ctxt envs e ty' =
    case typeSynthExp ctxt envs e of
        Left err -> Just err
        Right ty | subTypeOf (typeDefs envs) ty ty' -> Nothing
                 | otherwise ->
                    Just (ppType ctxt ty <> " is not a subtype of " <> ppType ctxt ty'
                       <> "\n  In expression: " <> ppExp ctxt e)

typeSynthExp
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id (Note nt t id)
    -> Either Out (Ty id (Note nt t id))
typeSynthExp ctxt envs e =
    case typeSynthExp' ctxt envs e of
        Left err -> Left (err <> "\n  In expression: " <> ppExp ctxt e)
        Right x -> Right x

typeSynthExp'
    :: (Show id, Ord id, Ord nt, Ord t, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id (Note nt t id)
    -> Either Out (Ty id (Note nt t id))
typeSynthExp' _ _ T = Right BoolTy
typeSynthExp' _ _ F = Right BoolTy
typeSynthExp' _ _ (U8 _) = Right U8Ty
typeSynthExp' _ _ (Int _) = Right IntTy
typeSynthExp' _ _ (Float _) = Right FloatTy
typeSynthExp' _ _ (String _) = Right StringTy
typeSynthExp' ctxt envs (If b t e) =
    case typeCheckExp ctxt envs b BoolTy of
        Just err -> Left err
        Nothing ->
            case (typeSynthExp ctxt envs t, typeSynthExp ctxt envs e) of
                (Left err, _) -> Left err
                (_, Left err) -> Left err
                (Right ty1, Right ty2) ->
                    case joinTy (typeDefs envs) ty1 ty2 of
                        Nothing -> Left (ppType ctxt ty1 <> " and " <> ppType ctxt ty2
                                      <> "don't have a common supertype")
                        Just ty -> Right ty
typeSynthExp' ctxt envs (Un op e) =
    case typeSynthExp ctxt envs e of
        Left err -> Left err
        Right ty ->
            case typeSynthUnOp envs op ty of
                Left err -> Left (err <> "\nType was: " <> ppType ctxt ty)
                r -> r
typeSynthExp' ctxt envs (Bin op e1 e2) =
    case typeSynthExp ctxt envs e1 of
        Left err -> Left err
        Right ty1 ->
            case typeSynthExp ctxt envs e2 of
                Left err -> Left err
                Right ty2 ->
                    case typeSynthBinOp ctxt envs op ty1 ty2 of
                        Left err ->
                            Left (err
                              <> "\nTypes were: " <> ppType ctxt ty1
                              <> "\nand: " <> ppType ctxt ty2)
                        r -> r
typeSynthExp' ctxt envs (Call f es) =
    case traverse (typeSynthExp ctxt envs) es of
        Left err -> Left err
        Right tys ->
            case Map.lookup f (funTypes envs) of
                Nothing -> Left ("Unknown function " <> tOut ctxt f)
                Just (tys', ty') ->
                    case typeCheckArgs ctxt (typeDefs envs) tys tys' of
                        Left (err, ix) ->
                            Left ("Type mismatch in arguments when calling " <> tOut ctxt f
                               <> ".\nArgument: " <> ppExp ctxt (es !! ix)
                               -- <> "\nExpected argument types: " <> ppList (ppType ctxt) tys'
                               -- <> "\nActual argument types: " <> ppList (ppType ctxt) tys
                               <> "\nError: " <> err)
                        Right bs -> Right (applyBindings bs ty')
typeSynthExp' ctxt envs (Annotate e ty) =
    case typeCheckExp ctxt envs e ty of
        Nothing -> Right ty
        Just err -> Left err
typeSynthExp' _ _ (Ref EOI) = Right IntTy
typeSynthExp' _ _ (Ref (Start _)) = Right IntTy
typeSynthExp' _ _ (Ref (End _)) = Right IntTy
typeSynthExp' ctxt envs (Ref (Id x)) =
    case Map.lookup x (locals envs) of
        Just ty -> Right ty
        _ -> case Map.lookup x (constTypes envs) of
                Just ty -> Right ty
                _ -> Left ("Unknown name " <> out ctxt x)
typeSynthExp' ctxt envs (Ref (Attr a@(nt, _) x))
    | x' == "this" =
        case Map.lookup a (localRuleTypes envs) of
            Just ty -> Right (Note nt ty)
            Nothing -> -- TODO: I don't think this fallback approach works in general.
                case Map.lookup nt (ruleTypes envs) of
                    Just (_, ty) -> Right (Note nt ty)
                    Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
    | x' == "these" =
        case Map.lookup a (localRuleTypes envs) of
            Just ty -> Right (ArrayTy (Note nt ty))
            Nothing -> -- TODO: I don't think this fallback approach works in general.
                case Map.lookup nt (ruleTypes envs) of
                    Just (_, ty) -> Right (Note nt ty)
                    Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthExp' ctxt envs (Ref (Attr a@(nt, _) x)) =
    case derefTy (typeDefs envs) <$> Map.lookup a (localRuleTypes envs) of
        Just ~ty'@(RowTy fs) ->
            case Map.lookup x fs of
                Just ty -> Right ty
                Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                 <> ppType ctxt ty')
        -- Just _ -> error "Rule with return type that isn't a row type."
        Nothing -> -- TODO: I don't think this fallback approach works in general.
            case fmap (derefTy (typeDefs envs)) <$> Map.lookup nt (ruleTypes envs) of
                Just (_, ~ty'@(RowTy fs)) ->
                    case Map.lookup x fs of
                        Just ty -> Right ty
                        Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                         <> ppType ctxt ty')
                Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
typeSynthExp' ctxt envs (Ref (Index a@(nt, _) e x))
    | x' == "this" =
        case typeCheckExp ctxt envs e IntTy of
            Just err -> Left err
            Nothing ->
                case Map.lookup a (localRuleTypes envs) of
                    Just ty -> Right (Note nt ty)
                    -- Just _ -> error "Rule with return type that isn't a row type."
                    Nothing -> -- TODO: I don't think this fallback approach works in general.
                        case Map.lookup nt (ruleTypes envs) of
                            Just (_, ty) -> Right (Note nt ty)
                            Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthExp' ctxt envs (Ref (Index a@(nt, _) e x)) =
    case typeCheckExp ctxt envs e IntTy of
        Just err -> Left err
        Nothing ->
            case derefTy (typeDefs envs) <$> Map.lookup a (localRuleTypes envs) of
                Just ~ty'@(RowTy fs) ->
                    case Map.lookup x fs of
                        Just ty -> Right ty
                        Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                         <> ppType ctxt ty')
                -- Just _ -> error "Rule with return type that isn't a row type."
                Nothing -> -- TODO: I don't think this fallback approach works in general.
                    case fmap (derefTy (typeDefs envs)) <$> Map.lookup nt (ruleTypes envs) of
                        Just (_, ~ty'@(RowTy fs)) ->
                            case Map.lookup x fs of
                                Just ty -> Right ty
                                Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                                 <> ppType ctxt ty')
                        Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)

-- TODO: These probably need type variable bindings inputs.
unCheck
    :: (Show id, Ord id, HasCallStack)
    => Environments nt t id
    -> Ty id (Note nt t id)
    -> Ty id (Note nt t id)
    -> Bool
unCheck envs ty ety = subTypeOf (typeDefs envs) ty ety

binCheck
    :: (Show id, Ord id, HasCallStack)
    => Environments nt t id
    -> Ty id (Note nt t id)
    -> Ty id (Note nt t id)
    -> Ty id (Note nt t id)
    -> Ty id (Note nt t id)
    -> Bool
binCheck envs ty1 ety1 ty2 ety2 = subTypeOf tyEnv ty1 ety1 && subTypeOf tyEnv ty2 ety2
  where tyEnv = typeDefs envs

typeSynthUnOp
    :: (Show id, Ord id, HasCallStack)
    => Environments nt t id
    -> UnOp
    -> Ty id (Note nt t id)
    -> Either Out (Ty id (Note nt t id))
typeSynthUnOp envs Not ty | unCheck envs ty BoolTy = Right BoolTy
typeSynthUnOp    _ Not  _ = Left "! expects a boolean argument"
typeSynthUnOp envs Neg ty | unCheck envs ty U8Ty = Right U8Ty
typeSynthUnOp envs Neg ty | unCheck envs ty IntTy = Right IntTy
typeSynthUnOp envs Neg ty | unCheck envs ty FloatTy = Right FloatTy
typeSynthUnOp    _ Neg  _ = Left "Negation expects a numeric argument"
typeSynthUnOp envs BitwiseNeg ty | unCheck envs ty BoolTy = Right BoolTy
typeSynthUnOp envs BitwiseNeg ty | unCheck envs ty U8Ty = Right U8Ty
typeSynthUnOp envs BitwiseNeg ty | unCheck envs ty IntTy = Right IntTy
typeSynthUnOp    _ BitwiseNeg  _ = Left "Bitwise negation expects an integer or boolean argument"

typeSynthBinOp
    :: (Show id, Ord id, HasCallStack)
    => Context nt t id
    -> Environments nt t id
    -> BinOp
    -> Ty id (Note nt t id)
    -> Ty id (Note nt t id)
    -> Either Out (Ty id (Note nt t id))
typeSynthBinOp ctxt envs LessThan ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable envs ty -> Right BoolTy
                | otherwise -> Left (ppType ctxt ty <> " isn't an orderable type")
        Nothing -> Left "< expects comparable types"
typeSynthBinOp ctxt envs LTE ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable envs ty -> Right BoolTy
                | otherwise -> Left (ppType ctxt ty <> " isn't an orderable type")
        Nothing -> Left "<= expects comparable types"
typeSynthBinOp ctxt envs GreaterThan ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable envs ty -> Right BoolTy
                | otherwise -> Left (ppType ctxt ty <> " isn't an orderable type")
        Nothing -> Left "> expects comparable types"
typeSynthBinOp ctxt envs GTE ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable envs ty -> Right BoolTy
                | otherwise -> Left (ppType ctxt ty <> " isn't an orderable type")
        Nothing -> Left ">= expects comparable types"
typeSynthBinOp ctxt envs Equal ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isEquatable envs ty -> Right BoolTy
                | otherwise -> Left (ppType ctxt ty <> " isn't an equatable type")
        Nothing -> Left "== expects comparable types"
typeSynthBinOp ctxt envs NotEqual ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isEquatable envs ty -> Right BoolTy
                | otherwise -> Left (ppType ctxt ty <> " isn't an equatable type")
        Nothing -> Left "!= expects comparable types"
typeSynthBinOp _ envs And ty1 ty2 | binCheck envs ty1 BoolTy ty2 BoolTy = Right BoolTy
typeSynthBinOp _    _ And _ _ = Left "&& expects boolean arguments"
typeSynthBinOp _ envs Or ty1 ty2 | binCheck envs ty1 BoolTy ty2 BoolTy = Right BoolTy
typeSynthBinOp _    _ Or _ _ = Left "|| expects boolean arguments"
typeSynthBinOp _ envs BitwiseAnd ty1 ty2 | binCheck envs ty1 BoolTy ty2 BoolTy = Right BoolTy
typeSynthBinOp _ envs BitwiseAnd ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs BitwiseAnd ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _    _ BitwiseAnd _ _ = Left "& expects matching boolean or integer arguments"
typeSynthBinOp _ envs BitwiseXor ty1 ty2 | binCheck envs ty1 BoolTy ty2 BoolTy = Right BoolTy
typeSynthBinOp _ envs BitwiseXor ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs BitwiseXor ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _    _ BitwiseXor _ _ = Left "^ expects matching boolean or integer arguments"
typeSynthBinOp _ envs BitwiseOr ty1 ty2 | binCheck envs ty1 BoolTy ty2 BoolTy = Right BoolTy
typeSynthBinOp _ envs BitwiseOr ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs BitwiseOr ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _    _ BitwiseOr _ _ = Left "| expects matching boolean or integer arguments"
typeSynthBinOp _ envs LSh ty1 ty2 | binCheck envs ty1 U8Ty ty2 IntTy = Right U8Ty
typeSynthBinOp _ envs LSh ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _    _ LSh _ _ = Left "<< expects integer arguments"
typeSynthBinOp _ envs RSh ty1 ty2 | binCheck envs ty1 U8Ty ty2 IntTy = Right U8Ty
typeSynthBinOp _ envs RSh ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _    _ RSh _ _ = Left ">> expects integer arguments"
typeSynthBinOp _ envs Add ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs Add ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _ envs Add ty1 ty2 | binCheck envs ty1 IntTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _ envs Add ty1 ty2 | binCheck envs ty1 FloatTy ty2 IntTy = Right FloatTy
typeSynthBinOp _ envs Add ty1 ty2 | binCheck envs ty1 FloatTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _ envs Add ty1 ty2 | binCheck envs ty1 StringTy ty2 StringTy = Right StringTy
typeSynthBinOp _    _ Add _ _ = Left "Addition expects matching numeric or string arguments"
typeSynthBinOp _ envs Sub ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs Sub ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _ envs Sub ty1 ty2 | binCheck envs ty1 IntTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _ envs Sub ty1 ty2 | binCheck envs ty1 FloatTy ty2 IntTy = Right FloatTy
typeSynthBinOp _ envs Sub ty1 ty2 | binCheck envs ty1 FloatTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _    _ Sub _ _ = Left "Subtraction expects numeric arguments"
typeSynthBinOp _ envs Mul ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs Mul ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _ envs Mul ty1 ty2 | binCheck envs ty1 FloatTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _    _ Mul _ _ = Left "Multiplication expects numeric arguments"
typeSynthBinOp _ envs Exp ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right FloatTy
typeSynthBinOp _ envs Exp ty1 ty2 | binCheck envs ty1 IntTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _ envs Exp ty1 ty2 | binCheck envs ty1 FloatTy ty2 IntTy = Right FloatTy
typeSynthBinOp _ envs Exp ty1 ty2 | binCheck envs ty1 FloatTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _    _ Exp _ _ = Left "Exponentiation expects numeric arguments"
typeSynthBinOp _ envs Div ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs Div ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _ envs Div ty1 ty2 | binCheck envs ty1 IntTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _ envs Div ty1 ty2 | binCheck envs ty1 FloatTy ty2 IntTy = Right FloatTy
typeSynthBinOp _ envs Div ty1 ty2 | binCheck envs ty1 FloatTy ty2 FloatTy = Right FloatTy
typeSynthBinOp _    _ Div _ _ = Left "Division expects numeric arguments"
typeSynthBinOp _ envs Mod ty1 ty2 | binCheck envs ty1 U8Ty ty2 U8Ty = Right U8Ty
typeSynthBinOp _ envs Mod ty1 ty2 | binCheck envs ty1 IntTy ty2 IntTy = Right IntTy
typeSynthBinOp _    _ Mod _ _ = Left "% expects integer arguments"
typeSynthBinOp _ envs At ty1 ty2 | binCheck envs ty1 StringTy ty2 IntTy = Right U8Ty
typeSynthBinOp _ envs At (ArrayTy ty) ty2 | unCheck envs ty2 IntTy = Right ty -- TODO
typeSynthBinOp _    _ At _ _ = Left "Indexing expects a string or sequence first argument and an integer second argument"
