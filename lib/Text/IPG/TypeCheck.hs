{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Text.IPG.TypeCheck (
    Context(..), ConstTypes, FunTypes, RuleTypes, TypeDefs, Bindings, Bindings',
    Environments(typeDefs, constTypes, funTypes, ruleTypes),
    annotate, fullyDereference, isEquatable, isOrderable, joinTy, subTypeOf, (<:), typeCheck, 
) where
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers

import Data.Maybe ( catMaybes, fromJust ) -- base -- TODO: Remove fromJust.

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

-- TODO: Consider these.
isEquatable :: Ty id -> Bool
isEquatable (ArrayTy _) = False
isEquatable (RowTy _) = False
isEquatable (ExternalTy _) = False -- TODO: Do I want to allow this?
isEquatable _ = True

isOrderable :: Ty id -> Bool
isOrderable (ArrayTy _) = False
isOrderable (RowTy _) = False
isOrderable (ExternalTy _) = False
isOrderable _ = True

aOut :: (nt -> Out) -> (nt, Int) -> Out
aOut ntOut' (nt, i) = ntOut' nt <> "@" <> Builder.intDec i

-- TODO: Using this willy-nilly is expensive.
fullyDereference :: (Ord id) => TypeDefs id -> Ty id -> Ty id
fullyDereference env = go Set.empty Set.empty
  where go seen guarded ty@(ExternalTy x)
            | x `Set.member` guarded = error "(Guarded) circular typedefs" -- TODO: For now.
            | x `Set.member` seen = error "Circular typedefs"
            -- | x `Set.member` guarded = ty
            | otherwise = go (Set.insert x seen) guarded (case Map.lookup x env of Just y -> y)
        go seen guarded (ArrayTy ty) = ArrayTy (go Set.empty (Set.union seen guarded) ty)
        go seen guarded (RowTy fs) = RowTy (go Set.empty (Set.union seen guarded) <$> fs)
        go _ _ ty = ty

-- TODO: We need to deref types during this unless we fully ground a type first.
(<:) :: (Ord id) => Ty id -> Ty id -> Bool
ty1 <: ty2 = subTypeOf Map.empty ty1 ty2

subTypeOf :: (Ord id) => TypeDefs id -> Ty id -> Ty id -> Bool
subTypeOf tyEnv ty ty' = fst (subTypeOf' tyEnv True Map.empty ty ty')

type Bindings' v id = Map.Map v (Ty' v id)
type Bindings id = Bindings' id id

subTypeOf'
    :: (Ord id, Ord v)
    => TypeDefs id
    -> Bool
    -> Bindings' v id
    -> Ty' v id
    -> Ty' v id
    -> (Bool, Bindings' v id)
subTypeOf' tyEnv rigid bs0 t1 t2 = go bs0 (derefTy tyEnv t1) (derefTy tyEnv t2)
    where go bs BoolTy BoolTy = (True, bs)
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
          deref _ ty@(ExternalTy _) = derefTy tyEnv ty
          deref bs ty@(TyVar v) =
            case Map.lookup v bs of
                Nothing -> ty
                Just ty' -> deref bs ty'
          deref _ ty = ty

groundType :: (Ord v) => Bindings' v id -> Ty' v id -> Ty' v id
groundType bs = mapTyVar f
    where f v =
            case Map.lookup v bs of
                Nothing -> TyVar v
                Just ty@(TyVar v') | v == v' -> ty
                Just ty -> groundType bs ty

typeCheckArgs :: (Ord id) => TypeDefs id -> [Ty id] ->  [Ty id] -> Maybe (Bindings id)
typeCheckArgs tyEnv lTys rTys = go Map.empty lTys' rTys'
  where lTys' = map (mapTyVar (TyVar . Left)) lTys
        rTys' = map (mapTyVar (TyVar . Right)) rTys
        f _ (Left _, _) = Nothing
        f bs (Right v, ty) = Just (v, mapTyVar (TyVar . either id id) (groundType bs ty))
        go _ [] (_:_) = error "Arity mismatch in typeCheckArgs"
        go _ (_:_) [] = error "Arity mismatch in typeCheckArgs"
        go bs [] [] = Just (Map.fromList (catMaybes (map (f bs) (Map.toList bs))))
        go bs (ty:tys) (ty':tys') =
            case subTypeOf' tyEnv False bs ty ty' of
                (False, _) -> Nothing
                (True, bs') -> go bs' tys tys'

applyBindings :: (Ord id) => Bindings id -> Ty id -> Ty id
applyBindings bs = mapTyVar go
    where go v = case Map.lookup v bs of Nothing -> TyVar v; Just ty -> ty

-- TODO: Handle type variables.
joinTy :: (Ord id) => TypeDefs id -> Ty id -> Ty id -> Maybe (Ty id)
joinTy _ ty@(ExternalTy x) (ExternalTy y) | x == y = Just ty
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

derefTy :: (Ord id) => TypeDefs id -> Ty' v id -> Ty' v id
derefTy tyEnv' = go Set.empty
    where go seen ty@(ExternalTy x)
            | x `Set.member` seen = error "Circular typedefs"
            | otherwise =
                case Map.lookup x tyEnv' of
                    Nothing -> ty
                    Just ty' -> go (Set.insert x seen) ty'
          go _ ty = ty

type TypeDefs' v id = Map.Map id (Ty' v id) -- TODO: Handle arguments.
type TypeDefs id = forall v. TypeDefs' v id
type ConstTypes id = Map.Map id (Ty id)
type FunTypes t id = Map.Map t ([Ty id], Ty id)
type RuleTypes nt id = Map.Map nt ([Ty id], Ty id)

data Environments nt t id = Environments {
    typeDefs :: TypeDefs id,
    constTypes :: ConstTypes id,
    funTypes :: FunTypes t id,
    ruleTypes :: RuleTypes nt id,
    locals :: Map.Map id (Ty id),
    localRuleTypes :: Map.Map (nt, Int) (Ty id)
  }

data Context nt t id = Context {
    currentRule :: nt,
    values :: id, 
    out :: id -> Out,
    tOut :: t -> Out,
    ntOut :: nt -> Out
  }

annotate :: (Ord id, Ord nt) => Environments nt t id -> Grammar nt t id e -> Grammar nt t id e
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
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Grammar nt t id (Exp nt t id)
    -> Either [Out] (Environments nt t id)
typeCheck ctxt (Grammar decls)
    | null cErrs && null rErrs = Right envs
    | otherwise = Left (cErrs <> rErrs)                
  where (rules, consts, tyEnv, ruleDecls, funDecls) = partitionDeclarations decls
        -- TODO: Need to handle mutual recursion and args.
        noTyVars = mapTyVar (\_ -> error "There should be no free type variables in typedefs.")
        tyEnv' = Map.fromList (map (\(n, _, ty) -> (n, noTyVars ty)) tyEnv)
        fTypes' = Map.fromList (map (\(n, tys, ty) -> (n, (map snd tys, ty))) funDecls)
        -- Add zero-arity rules that don't have explicit declarations so we don't need to write them.
        zeroArityRules = catMaybes
                            (map (\(Rule _ nt es _) -> if null es then Just nt else Nothing) rules)
        argTys' = Map.fromList (map (\(nt, es, mty) -> (nt, (map snd es, mty))) ruleDecls)
        argTys = Map.union argTys' (Map.fromList (map (\nt -> (nt, ([], Nothing))) zeroArityRules))
        (cErrs, cTypes) = typeCheckConsts ctxt envs [] Map.empty consts
        (rErrs, rTypes) = typeCheckRules ctxt envs argTys [] Map.empty rules
        envs = Environments {
                 typeDefs = tyEnv',
                 constTypes = cTypes,
                 funTypes = fTypes',
                 ruleTypes = rTypes,
                 locals = Map.empty,
                 localRuleTypes = Map.empty
               }

typeCheckConsts
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> [Out]
    -> ConstTypes id
    -> [(id, Maybe (Ty id), Exp nt t id)]
    -> ([Out], ConstTypes id)
typeCheckConsts _ _ eAcc acc [] = (eAcc, acc)
typeCheckConsts ctxt envs eAcc acc (c@(n, _, _):cs) =
    typeCheckConsts ctxt envs eAcc' (Map.insert n ty acc) cs
  where (eAcc', ty) = 
            case typeCheckConst ctxt envs c of
                Left errs -> (errs <> eAcc, error "TODO: typeCheckConsts")
                Right ty' -> (eAcc, ty')

typeCheckConst
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> (id, Maybe (Ty id), Exp nt t id)
    -> Either [Out] (Ty id)
typeCheckConst ctxt envs (n, Nothing, e) =
    case typeSynthExp ctxt envs e of
        Right ty -> Right ty
        Left err -> Left ["Failed to type check const " <> out ctxt n <> ".\nError: " <> err]
typeCheckConst ctxt envs (n, Just ty, e) =
    case typeCheckExp ctxt envs e ty of
        Nothing -> Right ty
        Just err -> Left ["Failed to type check const " <> out ctxt n <> ".\nError: " <> err]

typeCheckRules
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Map.Map nt ([Ty id], Maybe (Ty id))
    -> [Out]
    -> RuleTypes nt id
    -> [Rule nt t id (Exp nt t id)]
    -> ([Out], RuleTypes nt id)
typeCheckRules _ _ _ eAcc acc [] = (eAcc, acc)
typeCheckRules ctxt envs argTys eAcc acc (r@(Rule _ nt _ _):rs) =
    typeCheckRules ctxt envs argTys eAcc' (Map.insert nt (tys, ty) acc) rs
  where (tys, mty) = case Map.lookup nt argTys of Just y -> y
        (eAcc', ty) = 
            case typeCheckRule ctxt envs r tys mty of
                Left errs -> (errs <> eAcc, error "TODO: typeCheckRules")
                Right ty' -> (eAcc, ty')

typeCheckRule
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Rule nt t id (Exp nt t id)
    -> [Ty id]
    -> Maybe (Ty id)
    -> Either [Out] (Ty id)
typeCheckRule ctxt envs (Rule _ nt ps alts) tys ty =
    let envs' = envs { locals = Map.fromList (zip ps tys) } -- TODO: Put these in a separate Map?
    in combine <$> traverse (\(Alternative ts) -> typeCheckAlternative ctxt' envs' ts ty) alts
  where ctxt' = ctxt { currentRule = nt }
        combine = foldr1 (\t1 t2 -> fromJust (joinTy (typeDefs envs) t1 t2)) -- TODO: Handle errors better.

typeCheckAlternative
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> [Term nt t id (Exp nt t id)]
    -> Maybe (Ty id)
    -> Either [Out] (Ty id)
typeCheckAlternative ctxt envs ts Nothing =
    case typeSynthTerms ctxt envs ts of
        Left errs -> Left errs
        Right ty -> Right ty
typeCheckAlternative ctxt envs ts (Just ty') =
    case typeSynthTerms ctxt envs ts of
        Left errs -> Left errs
        Right ty | subTypeOf (typeDefs envs) ty ty' -> Right ty'
        Right ty -> Left [pprintType' (out ctxt) ty <> " does not match expected type "
                          <> pprintType' (out ctxt) ty' <> " in rule "
                          <> ntOut ctxt (currentRule ctxt) <> "."]

typeSynthTerms
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> [Term nt t id (Exp nt t id)]
    -> Either [Out] (Ty id)
typeSynthTerms ctxt envs = go Map.empty (localRuleTypes envs)
  where go rows _ [] = Right (RowTy rows)
        go rows ntbs (t:ts) =
            let envs' = envs {
                            -- TODO: Currently, this causes local fields to shadow parameters.
                            locals = Map.union rows (locals envs),
                            localRuleTypes = ntbs
                        }
            in case typeSynthTerm ctxt envs' rows t of
                Left errs -> Left (errs <> [
                    "\n  In term: " <> ppTerm ctxt t <>
                    "\n  In rule: " <> ntOut ctxt (currentRule ctxt)])
                Right (rows', ntbs') -> go rows' (Map.union ntbs' ntbs) ts

typeSynthTerm
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Map.Map id (Ty id)
    -> Term nt t id (Exp nt t id)
    -> Either [Out] (Map.Map id (Ty id), Map.Map (nt, Int) (Ty id))
typeSynthTerm ctxt envs rows (Terminal _ l r) =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) -> Right (rows, Map.empty)
typeSynthTerm ctxt envs rows (x := e) =
    case typeSynthExp ctxt envs e of
        Left err -> Left [err]
        Right ty -> Right (Map.insert x ty rows, Map.empty)
typeSynthTerm ctxt envs rows (Guard e) =
    case typeCheckExp ctxt envs e BoolTy of
        Just err -> Left [err]
        Nothing -> Right (rows, Map.empty)
typeSynthTerm ctxt envs rows (Any x e) =
    case typeCheckExp ctxt envs e IntTy of
        Just err -> Left [err]
        Nothing -> Right (Map.insert x IntTy rows, Map.empty)
typeSynthTerm ctxt envs rows (Slice x l r) =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) -> Right (Map.insert x StringTy rows, Map.empty)
typeSynthTerm ctxt envs rows (NonTerminal a@(nt, _) es l r) =
    (\(ty, _) -> (rows, Map.singleton a ty)) <$> typeCheckRuleInvoke ctxt envs nt es l r
typeSynthTerm ctxt envs rows (Array j s e a@(nt, _) es l r) =
    case (typeCheckExp ctxt envs s IntTy, typeCheckExp ctxt envs e IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) -> -- TODO: Probably need to do something for `these` here.
            let envs' = envs { locals = Map.insert j IntTy (locals envs) }
            in (\(ty, _) -> (rows, Map.singleton a ty))
                <$> typeCheckRuleInvoke ctxt envs' nt es l r
typeSynthTerm ctxt envs rows (Repeat a@(nt, _) es l r x l0 r0) =
    case (typeCheckExp ctxt envs l0 IntTy, typeCheckExp ctxt envs r0 IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) ->
            case typeCheckRuleInvoke ctxt envs nt es l r of
                Left errs -> Left errs
                Right (ty', fs)
                    | x' == "this" -> Right (Map.insert (values ctxt) (ArrayTy ty') rows,
                                             Map.singleton a ty')
                    | otherwise ->
                        case Map.lookup x fs of
                            Nothing -> Left [out ctxt x <> " is not a field on " <> ntOut ctxt nt]
                            Just ty -> Right (Map.insert (values ctxt) (ArrayTy ty) rows,
                                              Map.singleton a ty')
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthTerm ctxt envs rows (RepeatUntil a1@(nt1, _) es1 l r x l0 r0 a2@(nt2, _) es2) =
    case typeCheckRuleInvoke ctxt envs nt2 es2 l0 r0 of
        Left errs -> Left errs
        Right (ty2', _) ->
            case typeCheckRuleInvoke ctxt envs nt1 es1 l r of
                Left errs -> Left errs
                Right (ty1', fs)
                    | x' == "this" -> Right (Map.insert (values ctxt) (ArrayTy ty1') rows,
                                             Map.fromList [(a2, ty2'), (a1, ty1')])
                    | otherwise ->
                        case Map.lookup x fs of
                            Nothing -> Left [out ctxt x <> " is not a field on " <> ntOut ctxt nt1]
                            Just ty -> Right (Map.insert (values ctxt) (ArrayTy ty) rows,
                                              Map.fromList [(a2, ty2'), (a1, ty1')])
  where x' = Builder.toLazyByteString (out ctxt x)

typeCheckRuleInvoke
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> nt
    -> [Exp nt t id]
    -> Exp nt t id
    -> Exp nt t id
    -> Either [Out] (Ty id, Map.Map id (Ty id))
typeCheckRuleInvoke ctxt envs nt es l r =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) ->
            case traverse (typeSynthExp ctxt envs) es of
                Left err -> Left [err]
                Right tys ->
                    case Map.lookup nt (ruleTypes envs) of
                        Nothing -> Left ["Unknown rule " <> ntOut ctxt nt]
                        Just (tys', ty') ->
                            case typeCheckArgs (typeDefs envs) tys tys' of
                                Nothing -> Left ["Type mismatch in arguments when invoking "
                                                 <> ntOut ctxt nt]
                                Just bs ->
                                    let ty'' = applyBindings bs ty'
                                    in Right (ty'', fromRowTy (derefTy (typeDefs envs) ty''))
  where fromRowTy ~(RowTy fs) = fs

typeCheckExp
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id
    -> Ty id
    -> Maybe Out
typeCheckExp ctxt envs e ty' =
    case typeSynthExp ctxt envs e of
        Left err -> Just err
        Right ty | subTypeOf (typeDefs envs) ty ty' -> Nothing
                 | otherwise -> Just (pprintType' (out ctxt) ty <> " is not a subtype of "
                                        <> pprintType' (out ctxt) ty')

ppExp :: Context nt t id -> Exp nt t id -> Out
ppExp ctxt = pprintExpr 0 . trimap (toS . ntOut ctxt) (toS . tOut ctxt) (toS . out ctxt)
  where toS = LBS.toStrict . Builder.toLazyByteString

ppTerm :: Context nt t id -> Term nt t id (Exp nt t id) -> Out
ppTerm ctxt = pprintTerm . trimapTerm (toS . ntOut ctxt) (toS . tOut ctxt) (toS . out ctxt) 
                            (trimap (toS . ntOut ctxt) (toS . tOut ctxt) (toS . out ctxt))
  where toS = LBS.toStrict . Builder.toLazyByteString

typeSynthExp
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id
    -> Either Out (Ty id)
typeSynthExp ctxt envs e =
    case typeSynthExp' ctxt envs e of
        Left err -> Left (err <> "\n  In expression: " <> ppExp ctxt e)
        Right x -> Right x

typeSynthExp'
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id
    -> Either Out (Ty id)
typeSynthExp' _ _ T = Right BoolTy
typeSynthExp' _ _ F = Right BoolTy
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
                        Nothing -> Left (pprintType' (out ctxt) ty1 <> " and "
                                         <> pprintType' (out ctxt) ty2
                                         <> "don't have a common supertype")
                        Just ty -> Right ty
typeSynthExp' ctxt envs (Un op e) =
    case typeSynthExp ctxt envs e of
        Left err -> Left err
        Right ty -> typeSynthUnOp op ty
typeSynthExp' ctxt envs (Bin op e1 e2) =
    case typeSynthExp ctxt envs e1 of
        Left err -> Left err
        Right ty1 ->
            case typeSynthExp ctxt envs e2 of
                Left err -> Left err
                Right ty2 -> typeSynthBinOp ctxt envs op ty1 ty2
typeSynthExp' ctxt envs (Call f es) =
    case traverse (typeSynthExp ctxt envs) es of
        Left err -> Left err
        Right tys ->
            case Map.lookup f (funTypes envs) of
                Nothing -> Left ("Unknown function " <> tOut ctxt f)
                Just (tys', ty') ->
                    case typeCheckArgs (typeDefs envs) tys tys' of
                        Nothing -> Left ("Type mismatch in arguments when calling "
                                         <> tOut ctxt f)
                        Just bs -> Right (applyBindings bs ty')
typeSynthExp' _ _ (Ref EOI) = Right IntTy
typeSynthExp' _ _ (Ref (Start _)) = Right IntTy
typeSynthExp' _ _ (Ref (End _)) = Right IntTy
typeSynthExp' ctxt envs (Ref (Id x)) =
    case Map.lookup x (locals envs) of
        Just ty -> Right ty
        _ -> case Map.lookup x (constTypes envs) of
                Just ty -> Right ty
                _ -> Left ("Unknown name " <> out ctxt x)
typeSynthExp' ctxt envs (Ref (Attr a x))
    | x' == "this" =
        case Map.lookup a (localRuleTypes envs) of
            Just ty -> Right ty
            Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
    | x' == "these" =
        case Map.lookup a (localRuleTypes envs) of
            Just ty -> Right (ArrayTy ty)
            Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthExp' ctxt envs (Ref (Attr a x)) =
    case derefTy (typeDefs envs) <$> Map.lookup a (localRuleTypes envs) of
        Just ~ty'@(RowTy fs) ->
            case Map.lookup x fs of
                Just ty -> Right ty
                Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                 <> pprintType' (out ctxt) ty')
        -- Just _ -> error "Rule with return type that isn't a row type."
        Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
typeSynthExp' ctxt envs (Ref (Index a e x))
    | x' == "this" =
        case typeCheckExp ctxt envs e IntTy of
            Just err -> Left err
            Nothing ->
                case Map.lookup a (localRuleTypes envs) of
                    Just ty -> Right ty
                    -- Just _ -> error "Rule with return type that isn't a row type."
                    Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthExp' ctxt envs (Ref (Index a e x)) =
    case typeCheckExp ctxt envs e IntTy of
        Just err -> Left err
        Nothing ->
            case derefTy (typeDefs envs) <$> Map.lookup a (localRuleTypes envs) of
                Just ~ty'@(RowTy fs) ->
                    case Map.lookup x fs of
                        Just ty -> Right ty
                        Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                         <> pprintType' (out ctxt) ty')
                -- Just _ -> error "Rule with return type that isn't a row type."
                Nothing -> Left ("Unknown local rule binding " <> aOut (ntOut ctxt) a)

typeSynthUnOp :: UnOp -> Ty id -> Either Out (Ty id)
typeSynthUnOp Not BoolTy = Right BoolTy
typeSynthUnOp Not _ = Left "! expects a boolean argument"
typeSynthUnOp Neg IntTy = Right IntTy
typeSynthUnOp Neg FloatTy = Right FloatTy
typeSynthUnOp Neg _ = Left "Negation expects a numeric argument"
typeSynthUnOp BitwiseNeg BoolTy = Right BoolTy
typeSynthUnOp BitwiseNeg IntTy = Right IntTy
typeSynthUnOp BitwiseNeg _ = Left "Bitwise negation expects an integer or boolean argument"

typeSynthBinOp
    :: (Ord id)
    => Context nt t id
    -> Environments nt t id
    -> BinOp
    -> Ty id
    -> Ty id
    -> Either Out (Ty id)
typeSynthBinOp ctxt envs LessThan ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left "< expects comparable types"
typeSynthBinOp ctxt envs LTE ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left "<= expects comparable types"
typeSynthBinOp ctxt envs GreaterThan ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left "> expects comparable types"
typeSynthBinOp ctxt envs GTE ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left ">= expects comparable types"
typeSynthBinOp ctxt envs Equal ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isEquatable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an equatable type")
        Nothing -> Left "== expects comparable types"
typeSynthBinOp ctxt envs NotEqual ty1 ty2 =
    case joinTy (typeDefs envs) ty1 ty2 of
        Just ty | isEquatable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an equatable type")
        Nothing -> Left "!= expects comparable types"
typeSynthBinOp _ _ And BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ _ And _ _ = Left "&& expects boolean arguments"
typeSynthBinOp _ _ Or BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ _ Or _ _ = Left "|| expects boolean arguments"
typeSynthBinOp _ _ BitwiseAnd BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ _ BitwiseAnd IntTy IntTy = Right IntTy
typeSynthBinOp _ _ BitwiseAnd _ _ = Left "& expects matching boolean or integer arguments"
typeSynthBinOp _ _ BitwiseXor BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ _ BitwiseXor IntTy IntTy = Right IntTy
typeSynthBinOp _ _ BitwiseXor _ _ = Left "^ expects matching boolean or integer arguments"
typeSynthBinOp _ _ BitwiseOr BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ _ BitwiseOr IntTy IntTy = Right IntTy
typeSynthBinOp _ _ BitwiseOr _ _ = Left "| expects matching boolean or integer arguments"
typeSynthBinOp _ _ LSh IntTy IntTy = Right IntTy
typeSynthBinOp _ _ LSh _ _ = Left "<< expects integer arguments"
typeSynthBinOp _ _ RSh IntTy IntTy = Right IntTy
typeSynthBinOp _ _ RSh _ _ = Left ">> expects integer arguments"
typeSynthBinOp _ _ Add IntTy IntTy = Right IntTy
typeSynthBinOp _ _ Add IntTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Add FloatTy IntTy = Right FloatTy
typeSynthBinOp _ _ Add FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Add StringTy StringTy = Right StringTy
typeSynthBinOp _ _ Add _ _ = Left "Addition expects matching numeric or string arguments"
typeSynthBinOp _ _ Sub IntTy IntTy = Right IntTy
typeSynthBinOp _ _ Sub IntTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Sub FloatTy IntTy = Right FloatTy
typeSynthBinOp _ _ Sub FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Sub _ _ = Left "Subtraction expects numeric arguments"
typeSynthBinOp _ _ Mul IntTy IntTy = Right IntTy
typeSynthBinOp _ _ Mul IntTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Mul FloatTy IntTy = Right FloatTy
typeSynthBinOp _ _ Mul FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Mul _ _ = Left "Multiplication expects numeric arguments"
typeSynthBinOp _ _ Exp IntTy IntTy = Right FloatTy
typeSynthBinOp _ _ Exp IntTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Exp FloatTy IntTy = Right FloatTy
typeSynthBinOp _ _ Exp FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Exp _ _ = Left "Exponentiation expects numeric arguments"
typeSynthBinOp _ _ Div IntTy IntTy = Right IntTy
typeSynthBinOp _ _ Div IntTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Div FloatTy IntTy = Right FloatTy
typeSynthBinOp _ _ Div FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ _ Div _ _ = Left "Division expects numeric arguments"
typeSynthBinOp _ _ Mod IntTy IntTy = Right IntTy
typeSynthBinOp _ _ Mod _ _ = Left "% expects integer arguments"
typeSynthBinOp _ _ At StringTy IntTy = Right IntTy
typeSynthBinOp _ _ At (ArrayTy ty) IntTy = Right ty
typeSynthBinOp _ _ At _ _ = Left "Indexing expects a string or sequence first argument and an integer second argument"
