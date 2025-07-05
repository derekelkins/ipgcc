{-# LANGUAGE DeriveFunctor, Rank2Types #-}
module Text.IPG.Full (
    Grammar(..), Rule(..), Alternative(..), Term(..), StartingOn(..),
    ExpHelpers(..),
    toCore, toCoreRule, toCoreAlternative, toCoreTerm,
) where
import qualified Data.IntSet as IntSet -- containers
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers

import qualified Text.IPG.Core as Core

newtype Grammar nt t id e = Grammar [Either (Rule nt t id e) (id, e)]
    deriving ( Show )

instance Functor (Grammar nt t id) where
    fmap f (Grammar ruleOrConsts) =
        Grammar (map (either (Left . fmap f) (Right . fmap f)) ruleOrConsts)

-- A(a_1, ..., a_m) -> alt_1 / ... / alt_n;
data Rule nt t id e = Rule [Core.MetaTag] nt [id] [Alternative nt t id e]
    deriving ( Functor, Show )

 -- tm_1 ... tm_n
data Alternative nt t id e = Alternative [Term nt t id e]
    deriving ( Functor, Show )

data Term nt t id e
    = NonTerminal0 (nt, Int) [e]
        -- A@n(e_1, ..., e_m)
    | NonTerminal1 (nt, Int) [e] e
        -- A@n(e_1, ..., e_m)[e_l]
    | NonTerminal2 (nt, Int) [e] e e
        -- A@n(e_1, ..., e_m)[e_l, e_r]
    | Terminal0 t
        -- "foo"
    | Terminal1 t e
        -- "foo"[e_l]
    | Terminal2 t e e
        -- "foo"[e_l, e_r]
    | id := e
        -- { id = e }
    | Guard e
        -- ?[ e ]
    | Array id e e (nt, Int) [e] e e
        -- for id = e_1 to e_2 do A@n(e_1, ..., e_m)[e_l, e_r]
    | Any0 id
        -- { id = . }
    | Any1 id e
        -- { id = .[e] }
    | Slice0 id
        -- { id = * }
    | Slice1 id e
        -- { id = *[e_l] }
    | Slice2 id e e
        -- { id = *[e_l, e_r] }
    | Repeat0 (nt, Int) [e] id (StartingOn e)
        -- repeat A@n(a_1, ..., a_m).id starting on [e_l0, e_r0]
    | Repeat1 (nt, Int) [e] e id (StartingOn e)
        -- repeat A@n(a_1, ..., a_m)[e_l].id starting on [e_l0, e_r0]
    | Repeat2 (nt, Int) [e] e e id (StartingOn e)
        -- repeat A@n(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0]
    | RepeatUntil0 (nt, Int) [e] id (StartingOn e) (nt, Int) [e]
        -- repeat A@n(a_1, ..., a_m).id starting on [e_l0, e_r0] until B@m(b_1, ..., b_k)
    | RepeatUntil1 (nt, Int) [e] e id (StartingOn e) (nt, Int) [e]
        -- repeat A@n(a_1, ..., a_m)[e_l].id starting on [e_l0, e_r0] until B@m(b_1, ..., b_k)
    | RepeatUntil2 (nt, Int) [e] e e id (StartingOn e) (nt, Int) [e]
        -- repeat A@n(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0] until B@m(b_1, ..., b_k)
  deriving ( Functor, Show )

data StartingOn e
    = StartingOn0
    | StartingOn1 e
    | StartingOn2 e e
  deriving ( Functor, Show )

data ExpHelpers nt t id e = ExpHelpers {
    len :: t -> e,
    add :: e -> e -> e,
    num :: Int -> e,
    ref :: Core.Ref nt id e -> e,
    mapRef :: (Core.Ref nt id e -> Core.Ref nt id e) -> e -> e,
    crushRef :: forall m. (Monoid m) => (Core.Ref nt id e -> m) -> e -> m
  }

toCore
    :: (Ord id, Ord nt, Show nt)
    => ExpHelpers nt t id e
    -> Grammar nt t id e
    -> Core.Grammar nt t id e
toCore h (Grammar rules) = Core.Grammar (map (either (Left . toCoreRule h) Right) rules)

toCoreRule
    :: (Ord id, Ord nt, Show nt)
    => ExpHelpers nt t id e
    -> Rule nt t id e
    -> Core.Rule nt t id e
toCoreRule h (Rule mt nt args alts) = Core.Rule mt nt args (map (toCoreAlternative h) alts)

toCoreAlternative
    :: (Ord id, Ord nt, Show nt)
    => ExpHelpers nt t id e
    -> Alternative nt t id e
    -> Core.Alternative nt t id e
toCoreAlternative h (Alternative terms) =
    Core.Alternative (Core.rearrange g (Core.renumber (mapRef h) (go terms (num h 0) Map.empty [])))
  where go [] _ _ acc = reverse acc
        go (t:ts) nt seen acc = let (t', nt', seen') = toCoreTerm h nt seen t
                           in go ts nt' seen' (t':acc)
        g = crushRef h (Core.crushUses g (Set.singleton . Left) (Set.singleton . Right))

type NM nt = Map.Map nt IntSet.IntSet

freshen :: (Ord nt) => NM nt -> (nt, Int) -> (NM nt, (nt, Int))
freshen m (nt, -1) = (m', (nt, n))
    where n = maybe 0 (findNext 0) (Map.lookup nt m)
          m' = Map.insertWith IntSet.union nt (IntSet.singleton n) m
          findNext k s | k `IntSet.member` s = findNext (k + 1) s
                       | otherwise = k
freshen m x@(nt, n) = (Map.insertWith IntSet.union nt (IntSet.singleton n) m, x)

toCoreTerm
    :: (Ord nt)
    => ExpHelpers nt t id e
    -> e
    -> NM nt
    -> Term nt t id e
    -> (Core.Term nt t id e, e, NM nt)
toCoreTerm h p seen (NonTerminal0 nt args) =
    (Core.NonTerminal nt' args p (ref h Core.EOI), ref h (Core.End nt'), seen')
  where (seen', nt') = freshen seen nt
toCoreTerm h p seen (NonTerminal1 nt args l) =
    (Core.NonTerminal nt' args p (add h p l), ref h (Core.End nt'), seen')
  where (seen', nt') = freshen seen nt
toCoreTerm h _ seen (NonTerminal2 nt args l r) =
    (Core.NonTerminal nt' args l r, ref h (Core.End nt'), seen')
  where (seen', nt') = freshen seen nt
toCoreTerm h p seen (Terminal0 t) =
    (Core.Terminal t p (add h p (len h t)), add h p (len h t), seen)
toCoreTerm h p seen (Terminal1 t l) =
    (Core.Terminal t p (add h p l), add h p (len h t), seen)
toCoreTerm h _ seen (Terminal2 t l r) =
    (Core.Terminal t l r, add h l (len h t), seen)
toCoreTerm _ p seen (i := e) = (i Core.:= e, p, seen)
toCoreTerm _ p seen (Guard e) = (Core.Guard e, p, seen)
toCoreTerm h _ seen (Array i s e nt args l r) =
    (Core.Array i s e nt' args l r, ref h (Core.End nt'), seen')
  where (seen', nt') = freshen seen nt
toCoreTerm h p seen (Any0 i) = (Core.Any i p, add h p (num h 1), seen)
toCoreTerm h _ seen (Any1 i l) = (Core.Any i l, add h l (num h 1), seen)
toCoreTerm h p seen (Slice0 i) = (Core.Slice i p (ref h Core.EOI), ref h Core.EOI, seen)
toCoreTerm h p seen (Slice1 i l) = (Core.Slice i p (add h p l), (add h p l), seen)
toCoreTerm _ _ seen (Slice2 i l r) = (Core.Slice i l r, r, seen)
toCoreTerm h p seen (Repeat0 nt args i s) =
    (Core.Repeat nt' args (ref h (Core.End nt')) (ref h Core.EOI) i l0 r0,
     ref h (Core.End nt'),
     seen')
  where (l0, r0) = toCoreStartingOn h p s
        (seen', nt') = freshen seen nt
toCoreTerm h p seen (Repeat1 nt args l i s) =
    (Core.Repeat nt' args ntEnd (add h ntEnd l) i l0 r0, ntEnd, seen')
  where (l0, r0) = toCoreStartingOn h p s
        ntEnd = ref h (Core.End nt')
        (seen', nt') = freshen seen nt
toCoreTerm h p seen (Repeat2 nt args l r i s) =
    (Core.Repeat nt' args l r i l0 r0, ref h (Core.End nt'), seen')
  where (l0, r0) = toCoreStartingOn h p s
        (seen', nt') = freshen seen nt
toCoreTerm h p seen (RepeatUntil0 nt1 args1 i s nt2 args2) =
    (Core.RepeatUntil nt1' args1 nt1End (ref h Core.EOI) i l0 r0 nt2' args2,
     ref h (Core.End nt2'),
     seen'')
  where (l0, r0) = toCoreStartingOn h p s
        nt1End = ref h (Core.End nt1')
        (seen', nt1') = freshen seen nt1
        (seen'', nt2') = freshen seen' nt2
toCoreTerm h p seen (RepeatUntil1 nt1 args1 l i s nt2 args2) =
    (Core.RepeatUntil nt1' args1 nt1End (add h nt1End l) i l0 r0 nt2' args2,
     ref h (Core.End nt2'),
     seen'')
  where (l0, r0) = toCoreStartingOn h p s
        nt1End = ref h (Core.End nt1')
        (seen', nt1') = freshen seen nt1
        (seen'', nt2') = freshen seen' nt2
toCoreTerm h p seen (RepeatUntil2 nt1 args1 l r i s nt2 args2) =
    (Core.RepeatUntil nt1' args1 l r i l0 r0 nt2' args2, ref h (Core.End nt2'), seen'')
  where (l0, r0) = toCoreStartingOn h p s
        (seen', nt1') = freshen seen nt1
        (seen'', nt2') = freshen seen' nt2

toCoreStartingOn :: ExpHelpers nt t id e -> e -> StartingOn e -> (e, e)
toCoreStartingOn h p StartingOn0 = (p, ref h Core.EOI)
toCoreStartingOn h p (StartingOn1 l) = (p, add h p l)
toCoreStartingOn _ _ (StartingOn2 l r) = (l, r)
