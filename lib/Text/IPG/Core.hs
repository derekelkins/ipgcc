{-# LANGUAGE DeriveFunctor #-}
module Text.IPG.Core (
    Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..), MetaTag(..),
    nonTerminals, arrayNonTerminals, renumber, rearrange, crushUses
) where
import Data.List ( nub ) -- base
import qualified Data.Graph as G
import qualified Data.IntMap as IntMap -- containers
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers

data MetaTag = INSTRUMENT -- %instrument
    deriving ( Eq, Ord, Show )

newtype Grammar nt t id e = Grammar [Either (Rule nt t id e) (id, e)]
    deriving ( Show )

instance Functor (Grammar nt t id) where
    fmap f (Grammar ruleOrConsts) =
        Grammar (map (either (Left . fmap f) (Right . fmap f)) ruleOrConsts)

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

data Ref nt id e
    = Id id                 -- id, essentially self.id
    | Attr (nt, Int) id     -- A.id
    | Index (nt, Int) e id  -- A(e).id
    | EOI                   -- EOI
    | Start (nt, Int)       -- A.START
    | End (nt, Int)         -- A.END
  deriving ( Functor, Show )

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
    -> [Term nt t id e]
    -> [Term nt t id e]
rearrange uses' ts = map ((\(x, _, _) -> x ) . term) (G.reverseTopSort deps)
    where uses = crushTerm uses'
          defs = IntMap.fromDistinctAscList (zipWith (\j t -> (j, defines t)) [0..] ts)
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

defines :: (Ord id, Ord nt) => Term nt t id e -> Set.Set (Either id (nt, Int))
defines (NonTerminal nt _ _ _) = Set.singleton (Right nt)
defines (x := _) = Set.singleton (Left x)
defines (Array _ _ _ nt _ _ _) = Set.singleton (Right nt)
defines (Any x _) = Set.singleton (Left x)
defines (Slice x _ _) = Set.singleton (Left x)
defines (Repeat nt _ _ _ _ _ _) = Set.singleton (Right nt)
defines (RepeatUntil nt1 _ _ _ _ _ _ nt2 _) = Set.fromList [Right nt1, Right nt2]
defines _ = mempty

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
