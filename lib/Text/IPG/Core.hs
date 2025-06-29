{-# LANGUAGE DeriveFunctor #-}
module Text.IPG.Core ( 
    Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..), MetaTag(..),
    nonTerminals, nonArrayNonTerminals, arrayNonTerminals, renumber, rearrange,
) where
import Data.List ( nub ) -- base
import qualified Data.Map as Map -- containers

data MetaTag = INSTRUMENT -- %instrument
    deriving ( Eq, Ord, Show )

newtype Grammar nt t id e = Grammar [Rule nt t id e]
    deriving ( Functor, Show )

-- A(a_1, ..., a_m) -> alt_1 / ... / alt_n;
data Rule nt t id e = Rule [MetaTag] nt [id] [Alternative nt t id e]
    deriving ( Functor, Show )

-- tm_1 ... tm_n
data Alternative nt t id e = Alternative [Term nt t id e]
    deriving ( Functor, Show )

data Term nt t id e 
    = NonTerminal (nt, Int) [e] e e     -- A@n(a_1, ..., a_m)[e_l, e_r]
    | Terminal t e e                    -- s[e_l, e_r]
    | id := e                           -- {id = e}
    | Guard e                           -- ?[e]
    | Array id e e (nt, Int) [e] e e    -- for id=e_1 to e_2 do A@n(a_1, ..., a_m)[e_l, e_r]
    | Any id e                          -- {id = .[e]}
    | Slice id e e                      -- {id = *[l, r]}
    | Repeat (nt, Int) [e] e e id e e   -- repeat A@n(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0]
    | RepeatUntil (nt, Int) [e] e e id e e (nt, Int) [e]
                                        -- repeat A@n(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0]
                                        --     until B@m(b_1, ..., b_k)
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

rearrange :: [Term nt t id e] -> [Term nt t id e]
rearrange ts = ts -- TODO: Topologically sort based on data dependency order.

mapNT :: (e -> e') -> ((nt, Int) -> (nt', Int)) -> Ref nt id e -> Ref nt' id e'
mapNT _ _ (Id x) = Id x
mapNT _ f (Attr nt x) = Attr (f nt) x
mapNT g f (Index nt e x) = Index (f nt) (g e) x
mapNT _ _ EOI = EOI
mapNT _ f (Start nt) = Start (f nt)
mapNT _ f (End nt) = End (f nt)

nonArrayNonTerminals :: (Eq nt) => [Term nt t id e] -> [(nt, Int)]
nonArrayNonTerminals = nub . concatMap processTerm
    where processTerm (NonTerminal nt _ _ _) = [nt]
          processTerm (Repeat nt _ _ _ _ _ _) = [nt]
          processTerm (RepeatUntil nt1 _ _ _ _ _ _ nt2 _) = [nt1, nt2]
          processTerm _ = []

nonTerminals :: (Eq nt) => [Term nt t id e] -> [(nt, Int)]
nonTerminals = nub . concatMap processTerm
    where processTerm (NonTerminal nt _ _ _) = [nt]
          processTerm (Repeat nt _ _ _ _ _ _) = [nt]
          processTerm (RepeatUntil nt1 _ _ _ _ _ _ nt2 _) = [nt1, nt2]
          processTerm (Array _ _ _ nt _ _ _) = [nt]
          processTerm _ = []

arrayNonTerminals :: (Eq nt) => [Term nt t id e] -> [(nt, Int)]
arrayNonTerminals = nub . concatMap processTerm
    where processTerm (Array _ _ _ nt _ _ _) = [nt]
          processTerm _ = []

-- Also, perhaps add termination checker.
