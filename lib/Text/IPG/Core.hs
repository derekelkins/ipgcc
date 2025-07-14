{-# LANGUAGE DeriveFunctor #-}
module Text.IPG.Core (
    Ty, Ty'(..), Grammar(..), Declaration(..), Rule(..), Alternative(..), Term(..), Ref(..),
    MetaTag(..),
    nonTerminals, arrayNonTerminals, renumber, rearrange, crushUses, partitionDeclarations,
    foldDeclaration, mapTyVar, trimapRef, trimapTerm, externalizeType, externalizeDeclaration,
) where
import Data.List ( nub ) -- base
import qualified Data.Graph as G
import qualified Data.IntMap as IntMap -- containers
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers

data MetaTag
    = INSTRUMENT -- %instrument
    | EXPORT     -- %export
  deriving ( Eq, Ord, Show )

type Ty id = Ty' id id

data Ty' v id
    = BoolTy                        -- Bool
    | IntTy                         -- Int
    | FloatTy                       -- Float
    | StringTy                      -- String
    | RowTy (Map.Map id (Ty' v id)) -- { f_1: ty_1, ..., f_n: ty_n }
    | ArrayTy (Ty' v id)            -- [ty]
    | TyApp id [Ty' v id]           -- T(ty_1, ..., ty_n)
    | ExternalTy id                 -- External
    | TyVar v                       -- 'a
  deriving ( Show )

externalizeType :: (Ord id) => Set.Set id -> Ty' v id -> Ty' v id
externalizeType externals ty@(TyApp x [])
    | x `Set.member` externals = ExternalTy x
    | otherwise = ty
externalizeType _ ty = ty

mapTyVar :: (v -> Ty' v' id) -> Ty' v id -> Ty' v' id
mapTyVar f (TyVar v) = f v
mapTyVar f (RowTy fs) = RowTy (mapTyVar f <$> fs)
mapTyVar f (TyApp t xs) = TyApp t (mapTyVar f <$> xs)
mapTyVar f (ArrayTy ty) = ArrayTy (mapTyVar f ty)
mapTyVar _ BoolTy = BoolTy
mapTyVar _ IntTy = IntTy
mapTyVar _ FloatTy = FloatTy
mapTyVar _ StringTy = StringTy
mapTyVar _ (ExternalTy x) = ExternalTy x

newtype Grammar nt t id e = Grammar [Declaration Rule nt t id e]
    deriving ( Functor, Show )

data Declaration rule nt t id e
    = RuleDef (rule nt t id e)
    | ConstDeclaration id (Maybe (Ty id)) e            -- const id: ty = e;
    | TypeDeclaration id [id] (Ty id)                  -- type Foo(x_1, ..., x_n) = ty;
    | RuleDeclaration nt [(id, Ty id)] (Maybe (Ty id)) -- rule A(a_1: ty_1, ..., a_m: ty_m): ty;
    | FunctionDeclaration t [(id, Ty id)] (Ty id)      -- function f(a_1: ty_1, ..., a_m: ty_m): ty;
  deriving ( Functor, Show )

externalizeDeclaration
    :: (Ord id)
    => Set.Set id
    -> Declaration rule nt t id e
    -> Declaration rule nt t id e
externalizeDeclaration exts =
    foldDeclaration
        RuleDef
        (\x ty e -> ConstDeclaration x (externalizeType exts <$> ty) e)
        (\t args ty -> TypeDeclaration t args (externalizeType exts ty))
        (\nt args ty -> RuleDeclaration nt (map (fmap (externalizeType exts)) args)
                        (externalizeType exts <$> ty))
        (\f args ty -> FunctionDeclaration f args (externalizeType exts ty))

partitionDeclarations
    :: [Declaration rule nt t id e]
    -> ([rule nt t id e],
        [(id, Maybe (Ty id), e)],
        [(id, [id], Ty id)],
        [(nt, [(id, Ty id)], Maybe (Ty id))],
        [(t, [(id, Ty id)], Ty id)])
partitionDeclarations [] = ([], [], [], [], [])
partitionDeclarations (RuleDef r:ds) = (r:rs, cs, ts, rds, fs)
    where (rs, cs, ts, rds, fs) = partitionDeclarations ds
partitionDeclarations (ConstDeclaration x ty e:ds) = (rs, (x, ty, e):cs, ts, rds, fs)
    where (rs, cs, ts, rds, fs) = partitionDeclarations ds
partitionDeclarations (TypeDeclaration t args ty:ds) = (rs, cs, (t, args, ty):ts, rds, fs)
    where (rs, cs, ts, rds, fs) = partitionDeclarations ds
partitionDeclarations (RuleDeclaration nt args ty:ds) = (rs, cs, ts, (nt, args, ty):rds, fs)
    where (rs, cs, ts, rds, fs) = partitionDeclarations ds
partitionDeclarations (FunctionDeclaration f args ty:ds) = (rs, cs, ts, rds, (f, args, ty):fs)
    where (rs, cs, ts, rds, fs) = partitionDeclarations ds

foldDeclaration
    :: (rule nt t id e -> a)
    -> (id -> Maybe (Ty id) -> e -> a)
    -> (id -> [id] -> Ty id -> a)
    -> (nt -> [(id, Ty id)] -> Maybe (Ty id) -> a)
    -> (t -> [(id, Ty id)] -> Ty id -> a)
    -> Declaration rule nt t id e
    -> a
foldDeclaration ruleDef _ _ _ _ (RuleDef r) = ruleDef r
foldDeclaration _ constDecl _ _ _ (ConstDeclaration x ty e) = constDecl x ty e
foldDeclaration _ _ typeDecl _ _ (TypeDeclaration t args ty) = typeDecl t args ty
foldDeclaration _ _ _ ruleDecl _ (RuleDeclaration nt args ty) = ruleDecl nt args ty
foldDeclaration _ _ _ _ funDecl (FunctionDeclaration f args ty) = funDecl f args ty

-- A(a_1, ..., a_m) -> alt_1 / ... / alt_n;
data Rule nt t id e = Rule [MetaTag] nt [id] [Alternative nt t id e]
    deriving ( Functor, Show )

-- tm_1 ... tm_n
data Alternative nt t id e = Alternative [Term nt t id e]
    deriving ( Functor, Show )

data Term nt t id e
    = NonTerminal (nt, Int) [e] e e
        -- A@n(a_1, ..., a_m)[e_l, e_r]
    | Terminal t e e
        -- "foo"[e_l, e_r]
    | id := e
        -- { id = e }
    | Guard e
        -- ?[ e ]
    | Array id e e (nt, Int) [e] e e
        -- for id = e_1 to e_2 do A@n(a_1, ..., a_m)[e_l, e_r]
    | Any id e
        -- { id = .[e] }
    | Slice id e e
        -- { id = *[l, r] }
    | Repeat (nt, Int) [e] e e id e e
        -- repeat A@n(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0]
    | RepeatUntil (nt, Int) [e] e e id e e (nt, Int) [e]
        -- repeat A@n(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0] until B@m(b_1, ..., b_k)
  deriving ( Functor, Show )

trimapTerm
    :: (nt -> nt')
    -> (t -> t')
    -> (id -> id')
    -> (e -> e')
    -> Term nt t id e
    -> Term nt' t' id' e'
trimapTerm f _ _ k (NonTerminal (nt, i) es l r) = NonTerminal (f nt, i) (map k es) (k l) (k r)
trimapTerm _ g _ k (Terminal t l r) = Terminal (g t) (k l) (k r)
trimapTerm _ _ h k (x := e) = h x := k e
trimapTerm _ _ _ k (Guard e) = Guard (k e)
trimapTerm f _ h k (Array j s e (nt, i) es l r) =
    Array (h j) (k s) (k e) (f nt, i) (map k es) (k l) (k r)
trimapTerm _ _ h k (Any x e) = Any (h x) (k e)
trimapTerm _ _ h k (Slice x l r) = Slice (h x) (k l) (k r)
trimapTerm f _ h k (Repeat (nt, i) es l r x l0 r0) =
    Repeat (f nt, i) (map k es) (k l) (k r) (h x) (k l0) (k r0)
trimapTerm f _ h k (RepeatUntil (nt1, i1) es1 l r x l0 r0 (nt2, i2) es2) =
    RepeatUntil (f nt1, i1) (map k es1) (k l) (k r) (h x) (k l0) (k r0) (f nt2, i2) (map k es2)

data Ref nt id e
    = Id id                 -- id, essentially self.id
    | Attr (nt, Int) id     -- A.id
    | Index (nt, Int) e id  -- A(e).id
    | EOI                   -- EOI
    | Start (nt, Int)       -- A.START
    | End (nt, Int)         -- A.END
  deriving ( Functor, Show )

trimapRef :: (nt -> nt') -> (id -> id') -> (e -> e') -> Ref nt id e -> Ref nt' id' e'
trimapRef _ g _ (Id x) = Id (g x)
trimapRef f g _ (Attr (nt, i) x) = Attr (f nt, i) (g x)
trimapRef f g h (Index (nt, i) e x) = Index (f nt, i) (h e) (g x)
trimapRef _ _ _ EOI = EOI
trimapRef f _ _ (Start (nt, i)) = Start (f nt, i)
trimapRef f _ _ (End (nt, i)) = End (f nt, i)

renumber
    :: (Ord nt, Show nt)
    => ((Ref nt id e -> Ref nt id e) -> e -> e)
    -> [Term nt t id e]
    -> [Term nt t id e]
renumber mapRef terms = go Map.empty terms
    where go _ [] = []
          go seen (NonTerminal nt@(k, v) es l r:ts) =
            let f = mapRef (tnt seen)
            in NonTerminal nt (map f es) (f l) (f r):go (Map.insert k v seen) ts
          go seen (Terminal t l r:ts) =
            let f = mapRef (tnt seen)
            in Terminal t (f l) (f r):go seen ts
          go seen (x := e:ts) =
            let f = mapRef (tnt seen)
            in x := (f e):go seen ts
          go seen (Guard e:ts) =
            let f = mapRef (tnt seen)
            in Guard (f e):go seen ts
          go seen (Any x e:ts) =
            let f = mapRef (tnt seen)
            in Any x (f e):go seen ts
          go seen (Slice x l r:ts) =
            let f = mapRef (tnt seen)
            in Slice x (f l) (f r):go seen ts
          go seen (Array j s e nt@(k, v) es l r:ts) =
            let f = mapRef (tnt seen)
            in Array j (f s) (f e) nt (map f es) (f l) (f r):go (Map.insert k v seen) ts
          go seen (Repeat nt@(k, v) es l r x l0 r0:ts) =
            let f = mapRef (tnt seen)
            in Repeat nt (map f es) (f l) (f r) x (f l0) (f r0):go (Map.insert k v seen) ts
          go seen (RepeatUntil nt1@(k1, v1) es1 l r x l0 r0 nt2@(k2, v2) es2:ts) =
            let f = mapRef (tnt seen)
                seen' = Map.insert k1 v1 (Map.insert k2 v2 seen)
            in RepeatUntil nt1 (map f es1) (f l) (f r) x (f l0) (f r0) nt2 (map f es2):go seen' ts
          allNTs = Map.unionsWith (++)
                    (map (\(k, v) -> Map.singleton k [v]) (nonTerminals terms))
          tnt seen = let g = mapNT (mapRef g) h in g
            where h (nt, -1) =
                    case Map.lookup nt seen of
                        Just n -> (nt, n)
                        Nothing -> case nub <$> Map.lookup nt allNTs of
                                        Just [n] -> (nt, n) -- TODO: Better handling of this.
                                        _ -> error ("Ambiguous reference to " ++ show nt)
                  h nt = nt

-- TODO: Ensure that the terms are sorted in written order where possible.
rearrange
    :: (Ord id, Ord nt)
    => (e -> Set.Set (Either id (nt, Int)))
    -> id
    -> [Term nt t id e]
    -> [Term nt t id e]
rearrange uses' values ts = map ((\(x, _, _) -> x ) . term) (G.reverseTopSort deps)
    where uses = crushTerm uses'
          defs = IntMap.fromDistinctAscList (zipWith (\j t -> (j, defines values t)) [0..] ts)
          (deps, term) =
            G.graphFromEdges'
                (map
                    (\((i, t), jts) ->
                        let u = uses t
                            js = foldMap
                                    (\(j, _) ->
                                        if u `overlaps` (defs IntMap.! j) then [j] else [])
                                    jts
                        in (t, i, js))
                    (selects (zip [0..] ts)))

overlaps :: (Ord a) => Set.Set a -> Set.Set a -> Bool
overlaps x y = not (Set.null (Set.intersection x y))

selects :: [a] -> [(a, [a])]
selects [] = []
selects (x:xs) = (x, xs):map (\(y, ys) -> (y, x:ys)) (selects xs)

mapNT :: (e -> e') -> ((nt, Int) -> (nt', Int)) -> Ref nt id e -> Ref nt' id e'
mapNT _ _ (Id x) = Id x
mapNT _ f (Attr nt x) = Attr (f nt) x
mapNT g f (Index nt e x) = Index (f nt) (g e) x
mapNT _ _ EOI = EOI
mapNT _ f (Start nt) = Start (f nt)
mapNT _ f (End nt) = End (f nt)

crushUses :: (Monoid m) => (e -> m) -> (id -> m) -> ((nt, Int) -> m) -> Ref nt id e -> m
crushUses _ h _ (Id x) = h x
crushUses _ _ f (Attr nt _) = f nt
crushUses g _ f (Index nt e _) = f nt <> g e
crushUses _ _ f (Start nt) = f nt
crushUses _ _ f (End nt) = f nt
crushUses _ _ _ EOI = mempty

defines :: (Ord id, Ord nt) => id -> Term nt t id e -> Set.Set (Either id (nt, Int))
defines _ (NonTerminal nt _ _ _) = Set.singleton (Right nt)
defines _ (x := _) = Set.singleton (Left x)
defines _ (Array _ _ _ nt _ _ _) = Set.singleton (Right nt)
defines _ (Any x _) = Set.singleton (Left x)
defines _ (Slice x _ _) = Set.singleton (Left x)
defines v (Repeat nt _ _ _ _ _ _) = Set.fromList [Left v, Right nt]
defines v (RepeatUntil nt1 _ _ _ _ _ _ nt2 _) = Set.fromList [Left v, Right nt1, Right nt2]
defines _ _ = mempty

crushTerm :: (Monoid m) => (e -> m) -> Term nt t id e -> m
crushTerm f (NonTerminal _ es l r) = foldMap f es <> f l <> f r
crushTerm f (Terminal _ l r) = f l <> f r
crushTerm f (_ := e) = f e
crushTerm f (Guard e) = f e
crushTerm f (Array _ s e _ es l r) = f s <> f e <> foldMap f es <> f l <> f r
crushTerm f (Any _ e) = f e
crushTerm f (Slice _ l r) = f l <> f r
crushTerm f (Repeat _ es l r _ l0 r0) = foldMap f es <> f l <> f r <> f l0 <> f r0
crushTerm f (RepeatUntil _ es1 l r _ l0 r0 _ es2) =
    foldMap f es1 <> f l <> f r <> f l0 <> f r0 <> foldMap f es2

nonTerminals :: (Eq nt) => [Term nt t id e] -> [(nt, Int)]
nonTerminals = nub . foldMap processTerm
    where processTerm (NonTerminal nt _ _ _) = [nt]
          processTerm (Repeat nt _ _ _ _ _ _) = [nt]
          processTerm (RepeatUntil nt1 _ _ _ _ _ _ nt2 _) = [nt1, nt2]
          processTerm (Array _ _ _ nt _ _ _) = [nt]
          processTerm _ = []

arrayNonTerminals :: (Eq nt) => [Term nt t id e] -> [(nt, Int)]
arrayNonTerminals = nub . foldMap processTerm
    where processTerm (Array _ _ _ nt _ _ _) = [nt]
          processTerm _ = []

-- Also, perhaps add termination checker.
