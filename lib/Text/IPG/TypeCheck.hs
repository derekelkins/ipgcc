{-# LANGUAGE OverloadedStrings #-}
module Text.IPG.TypeCheck ( Context(..), typeCheck ) where
import qualified Data.ByteString.Builder as Builder -- bytestring
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers

import Text.IPG.Core (
    Ty(..), Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..),
    partitionDeclarations )
import Text.IPG.GenericExp ( BinOp(..), Exp(..), UnOp(..) )
import Text.IPG.PPrint ( Out, pprintType' )

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

-- TODO: Using this willy-nilly is expensive.
groundType :: (Ord id) => TypeEnv id -> Ty id -> Ty id
groundType env = go Set.empty Set.empty
  where go seen guarded ty@(ExternalTy x)
            | x `Set.member` guarded = error "(Guarded) circular typedefs" -- TODO: For now.
            | x `Set.member` seen = error "Circular typedefs"
            -- | x `Set.member` guarded = ty
            | otherwise = go (Set.insert x seen) guarded (env Map.! x)
        go seen guarded (ArrayTy ty) = ArrayTy (go Set.empty (Set.union seen guarded) ty)
        go seen guarded (RowTy fs) = RowTy (go Set.empty (Set.union seen guarded) <$> fs)
        go _ _ ty = ty

-- TODO: We need to deref types during this unless we fully ground a type first.
(<:) :: (Ord id) => Ty id -> Ty id -> Bool
BoolTy <: BoolTy = True
IntTy <: IntTy = True
IntTy <: FloatTy = True
FloatTy <: FloatTy = True
StringTy <: StringTy = True
ExternalTy x <: ExternalTy y = x == y
ArrayTy t <: ArrayTy t' = t <: t'
RowTy _ <: RowTy fs' | Map.null fs' = True
RowTy fs <: RowTy fs' =
    case foldMap (\(x', t') -> fmap (\t -> [(t, t')]) (Map.lookup x' fs)) (Map.toList fs') of
        Nothing -> False
        Just ps -> all (\(t, t') -> t <: t') ps
_ <: _ = False

joinTy :: (Ord id) => Ty id -> Ty id -> Maybe (Ty id)
joinTy (RowTy fs1) (RowTy fs2) =
    case traverse (\k -> (,) k <$> joinTy (fs1 Map.! k) (fs2 Map.! k)) (Set.toList commonFields) of
        Nothing -> Nothing
        Just fs -> Just (RowTy (Map.fromList fs))
  where commonFields = Set.intersection (Map.keysSet fs1) (Map.keysSet fs2)
joinTy (ArrayTy ty1) (ArrayTy ty2) =
    case joinTy ty1 ty2 of
        Nothing -> Nothing
        Just ty -> Just (ArrayTy ty)
joinTy ty1 ty2 | ty1 <: ty2 = Just ty2
               | ty2 <: ty1 = Just ty1
               | otherwise = Nothing

type TypeEnv id = Map.Map id (Ty id) -- TODO: Handle arguments.

-- derefTy :: (Ord id) => TypeEnv id -> Ty id -> Ty id
-- derefTy tyEnv' = go Set.empty
--     where go seen ty@(ExternalTy x)
--             | Set.member x seen = error "Circular typedefs"
--             | otherwise =
--                 case Map.lookup x tyEnv' of
--                     Nothing -> ty
--                     Just ty' -> go (Set.insert x seen) ty'
--           go _ ty = ty

type ConstTypes id = Map.Map id (Ty id)

type FunTypes t id = Map.Map t ([Ty id], Ty id)

type RuleDecls nt id = Map.Map nt ([Ty id], Ty id)

data Environments nt t id = Environments {
    tyEnv :: TypeEnv id,
    cTypes :: ConstTypes id,
    fTypes :: FunTypes t id,
    ruleDecls :: RuleDecls nt id,
    locals :: Map.Map id (Ty id)
  }

data Context nt t id = Context {
    currentRule :: nt,
    values :: id, 
    out :: id -> Out,
    tOut :: t -> Out,
    ntOut :: nt -> Out
  }

typeCheck
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Grammar nt t id (Exp nt t id)
    -> Maybe [Out]
typeCheck ctxt (Grammar decls) = foldMap (typeCheckRule ctxt envs) rules
    -- foldMap (typeCheckConst ctxt envs) consts <> foldMap (typeCheckRule ctxt envs ruleDecls) rules
  where (rules, consts, typeDefs, ruleDecls'', funDecls) = partitionDeclarations decls
        tyEnv' = Map.fromList (map (\(n, _, ty) -> (n, ty)) typeDefs) -- TODO: Need to handle mutual recursion and args.
        fTypes' = Map.fromList (map (\(n, tys, ty) -> (n, (map snd tys, ty))) funDecls)
        ruleDecls' =
            Map.fromList (map (\(n, tys, ty) -> (n, (map snd tys, groundType tyEnv' ty)))
                              ruleDecls'')
        cTypes' = Map.empty -- TODO: We could topologically sort consts and type check in that order. I'm not interested in *mutual* recursion here.
        envs = Environments {
                 tyEnv = tyEnv',
                 cTypes = cTypes', -- TODO: Problematic circularity.
                 fTypes = fTypes',
                 ruleDecls = ruleDecls',
                 locals = Map.empty
               }

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

typeCheckRule
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Rule nt t id (Exp nt t id)
    -> Maybe [Out]
typeCheckRule ctxt envs (Rule _ nt ps alts) =
    case Map.lookup nt (ruleDecls envs) of
        Just (tys, ty) ->
            let envs' = envs { locals = Map.fromList (zip ps tys) } -- TODO: Put these in a separate Map?
            in foldMap (\(Alternative ts) -> typeCheckAlternative ctxt' envs' ts ty) alts
        Nothing -> Just ["Unknown rule " <> ntOut ctxt nt]
  where ctxt' = ctxt { currentRule = nt }

typeCheckAlternative
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> [Term nt t id (Exp nt t id)]
    -> Ty id
    -> Maybe [Out]
typeCheckAlternative ctxt envs ts ty' =
    case typeSynthTerms ctxt envs ts of
        Left errs -> Just errs
        Right ty | ty <: ty' -> Nothing
        Right ty -> Just [pprintType' (out ctxt) ty <> " does not match expected type "
                          <> pprintType' (out ctxt) ty' <> " in rule "
                          <> ntOut ctxt (currentRule ctxt) <> "."]

typeSynthTerms
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> [Term nt t id (Exp nt t id)]
    -> Either [Out] (Ty id)
typeSynthTerms ctxt envs = go Map.empty
  where go rows [] = Right (RowTy rows)
        go rows (t:ts) =
            let envs' = envs { locals = Map.union rows (locals envs) } -- TODO: Currently, this causes local fields to shadow parameters.
            in case typeSynthTerm ctxt envs' rows t of
                Left errs -> Left errs
                Right rows' -> go rows' ts

typeSynthTerm
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Map.Map id (Ty id)
    -> Term nt t id (Exp nt t id)
    -> Either [Out] (Map.Map id (Ty id))
typeSynthTerm ctxt envs rows (Terminal _ l r) =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) -> Right rows
typeSynthTerm ctxt envs rows (x := e) =
    case typeSynthExp ctxt envs e of
        Left err -> Left [err]
        Right ty -> Right (Map.insert x ty rows)
typeSynthTerm ctxt envs rows (Guard e) =
    case typeCheckExp ctxt envs e BoolTy of
        Just err -> Left [err]
        Nothing -> Right rows
typeSynthTerm ctxt envs rows (Any x e) =
    case typeCheckExp ctxt envs e IntTy of
        Just err -> Left [err]
        Nothing -> Right (Map.insert x IntTy rows)
typeSynthTerm ctxt envs rows (Slice x l r) =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) -> Right (Map.insert x StringTy rows)
typeSynthTerm ctxt envs rows (NonTerminal (nt, _) es l r) =
    const rows <$> typeCheckRuleInvoke ctxt envs nt es l r
typeSynthTerm ctxt envs rows (Array j s e (nt, _) es l r) =
    case (typeCheckExp ctxt envs s IntTy, typeCheckExp ctxt envs e IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) -> -- TODO: Probably need to do something for `these` here.
            let envs' = envs { locals = Map.insert j IntTy (locals envs) }
            in const rows <$> typeCheckRuleInvoke ctxt envs' nt es l r
typeSynthTerm ctxt envs rows (Repeat (nt, _) es l r x l0 r0) =
    case (typeCheckExp ctxt envs l0 IntTy, typeCheckExp ctxt envs r0 IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) ->
            case typeCheckRuleInvoke ctxt envs nt es l r of
                Left errs -> Left errs
                Right fs ->
                    case Map.lookup x fs of
                        Nothing -> Left [out ctxt x <> " is not a field on " <> ntOut ctxt nt]
                        Just ty -> Right (Map.insert (values ctxt) (ArrayTy ty) rows)
typeSynthTerm ctxt envs rows (RepeatUntil (nt1, _) es1 l r x l0 r0 (nt2, _) es2) =
    case typeCheckRuleInvoke ctxt envs nt2 es2 l0 r0 of
        Left errs -> Left errs
        Right _ ->
            case typeCheckRuleInvoke ctxt envs nt1 es1 l r of
                Left errs -> Left errs
                Right fs ->
                    case Map.lookup x fs of
                        Nothing -> Left [out ctxt x <> " is not a field on " <> ntOut ctxt nt1]
                        Just ty -> Right (Map.insert (values ctxt) (ArrayTy ty) rows)

typeCheckRuleInvoke
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> nt
    -> [Exp nt t id]
    -> Exp nt t id
    -> Exp nt t id
    -> Either [Out] (Map.Map id (Ty id))
typeCheckRuleInvoke ctxt envs nt es l r =
    case (typeCheckExp ctxt envs l IntTy, typeCheckExp ctxt envs r IntTy) of
        (Just lerr, Just rerr) -> Left [lerr, rerr]
        (Just lerr, _) -> Left [lerr]
        (_, Just rerr) -> Left [rerr]
        (Nothing, Nothing) ->
            case traverse (typeSynthExp ctxt envs) es of
                Left err -> Left [err]
                Right tys ->
                    case Map.lookup nt (ruleDecls envs) of
                        Nothing -> Left ["Unknown rule " <> ntOut ctxt nt]
                        Just (tys', RowTy fs)
                            | and (zipWith (<:) tys (map (groundType (tyEnv envs)) tys')) ->
                                Right fs
                            | otherwise -> Left ["Type mismatch in arguments when calling "
                                                 <> ntOut ctxt nt]
                        Just _ -> error "Rule with return type that isn't a row type."

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
        Right ty | ty <: ty' -> Nothing
                 | otherwise -> Just (pprintType' (out ctxt) ty <> " is not a subtype of "
                                        <> pprintType' (out ctxt) ty')

-- typeSynthExp (should) always return dereferenced types.
typeSynthExp
    :: (Ord id, Ord nt, Ord t)
    => Context nt t id
    -> Environments nt t id
    -> Exp nt t id
    -> Either Out (Ty id)
typeSynthExp _ _ T = Right BoolTy
typeSynthExp _ _ F = Right BoolTy
typeSynthExp _ _ (Int _) = Right IntTy
typeSynthExp _ _ (Float _) = Right FloatTy
typeSynthExp _ _ (String _) = Right StringTy
typeSynthExp ctxt envs (If b t e) =
    case typeCheckExp ctxt envs b BoolTy of
        Just err -> Left err
        Nothing ->
            case (typeSynthExp ctxt envs t, typeSynthExp ctxt envs e) of
                (Left err, _) -> Left err
                (_, Left err) -> Left err
                (Right ty1, Right ty2) ->
                    case joinTy ty1 ty2 of
                        Nothing -> Left (pprintType' (out ctxt) ty1 <> " and "
                                         <> pprintType' (out ctxt) ty2
                                         <> "don't have a common supertype")
                        Just ty -> Right ty
typeSynthExp ctxt envs (Un op e) =
    case typeSynthExp ctxt envs e of
        Left err -> Left err
        Right ty -> typeSynthUnOp op ty
typeSynthExp ctxt envs (Bin op e1 e2) =
    case typeSynthExp ctxt envs e1 of
        Left err -> Left err
        Right ty1 ->
            case typeSynthExp ctxt envs e2 of
                Left err -> Left err
                Right ty2 -> typeSynthBinOp ctxt op ty1 ty2
typeSynthExp ctxt envs (Call f es) =
    case traverse (typeSynthExp ctxt envs) es of
        Left err -> Left err
        Right tys ->
            case Map.lookup f (fTypes envs) of
                Nothing -> Left ("Unknown function " <> tOut ctxt f)
                Just (tys', ty) | and (zipWith (<:) tys (map (groundType (tyEnv envs)) tys')) ->
                                    Right (groundType (tyEnv envs) ty)
                                | otherwise -> Left ("Type mismatch in arguments when calling "
                                                     <> tOut ctxt f)
typeSynthExp _ _ (Ref EOI) = Right IntTy
typeSynthExp _ _ (Ref (Start _)) = Right IntTy
typeSynthExp _ _ (Ref (End _)) = Right IntTy
typeSynthExp ctxt envs (Ref (Id x)) =
    case Map.lookup x (locals envs) of
        Just ty -> Right (groundType (tyEnv envs) ty)
        _ -> case Map.lookup x (cTypes envs) of
                Just ty -> Right (groundType (tyEnv envs) ty)
                _ -> Left ("Unknown name " <> out ctxt x)
typeSynthExp ctxt envs (Ref (Attr (nt, _) x))
    | x' == "this" =
        case Map.lookup nt (ruleDecls envs) of
            Just (_, ty@(RowTy _)) -> Right ty
            Just _ -> error "Rule with return type that isn't a row type."
            Nothing -> Left ("Unknown rule " <> ntOut ctxt nt)
    | x' == "these" =
        case Map.lookup nt (ruleDecls envs) of
            Just (_, ty@(RowTy _)) -> Right (ArrayTy ty)
            Just _ -> error "Rule with return type that isn't a row type."
            Nothing -> Left ("Unknown rule " <> ntOut ctxt nt)
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthExp ctxt envs (Ref (Attr (nt, _) x)) =
    case Map.lookup nt (ruleDecls envs) of
        Just (_, ty'@(RowTy fs)) ->
            case Map.lookup x fs of
                Just ty -> Right (groundType (tyEnv envs) ty)
                Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                 <> pprintType' (out ctxt) ty')
        Just _ -> error "Rule with return type that isn't a row type."
        Nothing -> Left ("Unknown rule " <> ntOut ctxt nt)
typeSynthExp ctxt envs (Ref (Index (nt, _) e x))
    | x' == "this" =
        case typeCheckExp ctxt envs e IntTy of
            Just err -> Left err
            Nothing ->
                case Map.lookup nt (ruleDecls envs) of
                    Just (_, ty@(RowTy _)) -> Right ty
                    Just _ -> error "Rule with return type that isn't a row type."
                    Nothing -> Left ("Unknown rule " <> ntOut ctxt nt)
  where x' = Builder.toLazyByteString (out ctxt x)
typeSynthExp ctxt envs (Ref (Index (nt, _) e x)) =
    case typeCheckExp ctxt envs e IntTy of
        Just err -> Left err
        Nothing ->
            case Map.lookup nt (ruleDecls envs) of
                Just (_, ty'@(RowTy fs)) ->
                    case Map.lookup x fs of
                        Just ty -> Right (groundType (tyEnv envs) ty)
                        Nothing -> Left ("Field " <> out ctxt x <> " not a field on type "
                                         <> pprintType' (out ctxt) ty')
                Just _ -> error "Rule with return type that isn't a row type."
                Nothing -> Left ("Unknown rule " <> ntOut ctxt nt)

typeSynthUnOp :: UnOp -> Ty id -> Either Out (Ty id)
typeSynthUnOp Not BoolTy = Right BoolTy
typeSynthUnOp Not _ = Left "! expects a boolean argument"
typeSynthUnOp Neg IntTy = Right IntTy
typeSynthUnOp Neg FloatTy = Right FloatTy
typeSynthUnOp Neg _ = Left "Negation expects a numeric argument"
typeSynthUnOp BitwiseNeg BoolTy = Right BoolTy
typeSynthUnOp BitwiseNeg IntTy = Right IntTy
typeSynthUnOp BitwiseNeg _ = Left "Bitwise negation expects an integer or boolean argument"

typeSynthBinOp :: (Ord id) => Context nt t id -> BinOp -> Ty id -> Ty id -> Either Out (Ty id)
typeSynthBinOp ctxt LessThan ty1 ty2 =
    case joinTy ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left "< expects comparable types"
typeSynthBinOp ctxt LTE ty1 ty2 =
    case joinTy ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left "<= expects comparable types"
typeSynthBinOp ctxt GreaterThan ty1 ty2 =
    case joinTy ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left "> expects comparable types"
typeSynthBinOp ctxt GTE ty1 ty2 =
    case joinTy ty1 ty2 of
        Just ty | isOrderable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an orderable type")
        Nothing -> Left ">= expects comparable types"
typeSynthBinOp ctxt Equal ty1 ty2 =
    case joinTy ty1 ty2 of
        Just ty | isEquatable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an equatable type")
        Nothing -> Left "== expects comparable types"
typeSynthBinOp ctxt NotEqual ty1 ty2 =
    case joinTy ty1 ty2 of
        Just ty | isEquatable ty -> Right BoolTy
                | otherwise -> Left (pprintType' (out ctxt) ty <> " isn't an equatable type")
        Nothing -> Left "!= expects comparable types"
typeSynthBinOp _ And BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ And _ _ = Left "&& expects boolean arguments"
typeSynthBinOp _ Or BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ Or _ _ = Left "|| expects boolean arguments"
typeSynthBinOp _ BitwiseAnd BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ BitwiseAnd IntTy IntTy = Right IntTy
typeSynthBinOp _ BitwiseAnd _ _ = Left "& expects matching boolean or integer arguments"
typeSynthBinOp _ BitwiseXor BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ BitwiseXor IntTy IntTy = Right IntTy
typeSynthBinOp _ BitwiseXor _ _ = Left "^ expects matching boolean or integer arguments"
typeSynthBinOp _ BitwiseOr BoolTy BoolTy = Right BoolTy
typeSynthBinOp _ BitwiseOr IntTy IntTy = Right IntTy
typeSynthBinOp _ BitwiseOr _ _ = Left "| expects matching boolean or integer arguments"
typeSynthBinOp _ LSh IntTy IntTy = Right IntTy
typeSynthBinOp _ LSh _ _ = Left "<< expects integer arguments"
typeSynthBinOp _ RSh IntTy IntTy = Right IntTy
typeSynthBinOp _ RSh _ _ = Left ">> expects integer arguments"
typeSynthBinOp _ Add IntTy IntTy = Right IntTy
typeSynthBinOp _ Add IntTy FloatTy = Right FloatTy
typeSynthBinOp _ Add FloatTy IntTy = Right FloatTy
typeSynthBinOp _ Add FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ Add StringTy StringTy = Right StringTy
typeSynthBinOp _ Add _ _ = Left "Addition expects matching numeric or string arguments"
typeSynthBinOp _ Sub IntTy IntTy = Right IntTy
typeSynthBinOp _ Sub IntTy FloatTy = Right FloatTy
typeSynthBinOp _ Sub FloatTy IntTy = Right FloatTy
typeSynthBinOp _ Sub FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ Sub _ _ = Left "Subtraction expects numeric arguments"
typeSynthBinOp _ Mul IntTy IntTy = Right IntTy
typeSynthBinOp _ Mul IntTy FloatTy = Right FloatTy
typeSynthBinOp _ Mul FloatTy IntTy = Right FloatTy
typeSynthBinOp _ Mul FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ Mul _ _ = Left "Multiplication expects numeric arguments"
typeSynthBinOp _ Exp IntTy IntTy = Right FloatTy
typeSynthBinOp _ Exp IntTy FloatTy = Right FloatTy
typeSynthBinOp _ Exp FloatTy IntTy = Right FloatTy
typeSynthBinOp _ Exp FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ Exp _ _ = Left "Exponentiation expects numeric arguments"
typeSynthBinOp _ Div IntTy IntTy = Right IntTy
typeSynthBinOp _ Div IntTy FloatTy = Right FloatTy
typeSynthBinOp _ Div FloatTy IntTy = Right FloatTy
typeSynthBinOp _ Div FloatTy FloatTy = Right FloatTy
typeSynthBinOp _ Div _ _ = Left "Division expects numeric arguments"
typeSynthBinOp _ Mod IntTy IntTy = Right IntTy
typeSynthBinOp _ Mod _ _ = Left "% expects integer arguments"
typeSynthBinOp _ At StringTy IntTy = Right IntTy
typeSynthBinOp _ At (ArrayTy ty) IntTy = Right ty
typeSynthBinOp _ At _ _ = Left "Indexing expects a string or sequence first argument and an integer second argument"
