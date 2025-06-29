{-# LANGUAGE DeriveFunctor #-}
module Text.IPG.Full ( 
    Grammar(..), Rule(..), Alternative(..), Term(..), StartingOn(..),
    ExpHelpers(..),
    toCore, toCoreRule, toCoreAlternative, toCoreTerm,
) where
import qualified Text.IPG.Core as Core

newtype Grammar nt t id e = Grammar [Rule nt t id e]
    deriving ( Functor, Show )

-- A(a_1, ..., a_m) -> alt_1 / ... / alt_n;
data Rule nt t id e = Rule [Core.MetaTag] nt [id] [Alternative nt t id e]
    deriving ( Functor, Show )

 -- tm_1 ... tm_n
data Alternative nt t id e = Alternative [Term nt t id e]
    deriving ( Functor, Show )

data Term nt t id e 
    = NonTerminal0 nt [e]           -- A{e_1, ..., e_m}
    | NonTerminal1 nt [e] e         -- A{e_1, ..., e_m}[e_l]
    | NonTerminal2 nt [e] e e       -- A{e_1, ..., e_m}[e_l, e_r]
    | Terminal0 t                   -- s
    | Terminal1 t e                 -- s[e_l]
    | Terminal2 t e e               -- s[e_l, e_r]
    | id := e                       -- {id = e}
    | Guard e                       -- ?[e]
    | Array id e e nt [e] e e       -- for id=e_1 to e_2 do A(e_1, ..., e_m)[e_l, e_r]
    | Any0 id                       -- {id = .}
    | Any1 id e                     -- {id = .[e]}
    | Slice0 id                     -- {id = *}
    | Slice1 id e                   -- {id = *[e_l]}
    | Slice2 id e e                 -- {id = *[e_l, e_r]}
    | Repeat0 nt [e] id (StartingOn e)
                                    -- repeat A(a_1, ..., a_m).id starting on [e_l0, e_r0]
    | Repeat1 nt [e] e id (StartingOn e)
                                    -- repeat A(a_1, ..., a_m)[e_l].id starting on [e_l0, e_r0]
    | Repeat2 nt [e] e e id (StartingOn e)
                                    -- repeat A(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0]
    | RepeatUntil0 nt [e] id (StartingOn e) nt [e]
                                    -- repeat A(a_1, ..., a_m).id starting on [e_l0, e_r0]
                                    --     until B(b_1, ..., b_k)
    | RepeatUntil1 nt [e] e id (StartingOn e) nt [e]
                                    -- repeat A(a_1, ..., a_m)[e_l].id starting on [e_l0, e_r0]
                                    --     until B(b_1, ..., b_k)
    | RepeatUntil2 nt [e] e e id (StartingOn e) nt [e]
                                    -- repeat A(a_1, ..., a_m)[e_l, e_r].id starting on [e_l0, e_r0]
                                    --     until B(b_1, ..., b_k)
    -- TODO: Switch
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
    ref :: Core.Ref nt id e -> e
  }

toCore :: ExpHelpers nt t id e -> Grammar nt t id e -> Core.Grammar nt t id e
toCore h (Grammar rules) = Core.Grammar (map (toCoreRule h) rules)

toCoreRule :: ExpHelpers nt t id e -> Rule nt t id e -> Core.Rule nt t id e
toCoreRule h (Rule mt nt args alts) = Core.Rule mt nt args (map (toCoreAlternative h) alts)

toCoreAlternative :: ExpHelpers nt t id e -> Alternative nt t id e -> Core.Alternative nt t id e
toCoreAlternative h (Alternative terms) =
    Core.Alternative (go terms (num h 0) [])
  where go [] _ acc = reverse acc
        go (t:ts) nt acc = let (t', nt') = toCoreTerm h nt t
                           in go ts nt' (t':acc)

toCoreTerm :: ExpHelpers nt t id e -> e -> Term nt t id e -> (Core.Term nt t id e, e)
toCoreTerm h p (NonTerminal0 nt args) =
    (Core.NonTerminal nt args p (ref h Core.EOI), ref h (Core.End nt))
toCoreTerm h p (NonTerminal1 nt args l) =
    (Core.NonTerminal nt args p (add h p l), ref h (Core.End nt))
toCoreTerm h _ (NonTerminal2 nt args l r) =
    (Core.NonTerminal nt args l r, ref h (Core.End nt))
toCoreTerm h p (Terminal0 t) = (Core.Terminal t p (add h p (len h t)), add h p (len h t))
toCoreTerm h p (Terminal1 t l) = (Core.Terminal t p (add h p l), add h p (len h t))
toCoreTerm h _ (Terminal2 t l r) = (Core.Terminal t l r, add h l (len h t))
toCoreTerm _ p (i := e) = (i Core.:= e, p)
toCoreTerm _ p (Guard e) = (Core.Guard e, p)
toCoreTerm h _ (Array i s e nt args l r) =
    (Core.Array i s e nt args l r, ref h (Core.End nt))
toCoreTerm h p (Any0 i) = (Core.Any i p, add h p (num h 1))
toCoreTerm h _ (Any1 i l) = (Core.Any i l, add h l (num h 1))
toCoreTerm h p (Slice0 i) = (Core.Slice i p (ref h Core.EOI), ref h Core.EOI)
toCoreTerm h p (Slice1 i l) = (Core.Slice i p (add h p l), (add h p l))
toCoreTerm _ _ (Slice2 i l r) = (Core.Slice i l r, r)
toCoreTerm h p (Repeat0 nt args i s) =
    (Core.Repeat nt args (ref h (Core.End nt)) (ref h Core.EOI) i l0 r0, ref h (Core.End nt))
  where (l0, r0) = toCoreStartingOn h p s
toCoreTerm h p (Repeat1 nt args l i s) =
    (Core.Repeat nt args ntEnd (add h ntEnd l) i l0 r0, ntEnd)
  where (l0, r0) = toCoreStartingOn h p s
        ntEnd = ref h (Core.End nt)
toCoreTerm h p (Repeat2 nt args l r i s) =
    (Core.Repeat nt args l r i l0 r0, ref h (Core.End nt))
  where (l0, r0) = toCoreStartingOn h p s
toCoreTerm h p (RepeatUntil0 nt1 args1 i s nt2 args2) =
    (Core.RepeatUntil nt1 args1 nt1End (ref h Core.EOI) i l0 r0 nt2 args2, ref h (Core.End nt2))
  where (l0, r0) = toCoreStartingOn h p s
        nt1End = ref h (Core.End nt1)
toCoreTerm h p (RepeatUntil1 nt1 args1 l i s nt2 args2) =
    (Core.RepeatUntil nt1 args1 nt1End (add h nt1End l) i l0 r0 nt2 args2, ref h (Core.End nt2))
  where (l0, r0) = toCoreStartingOn h p s
        nt1End = ref h (Core.End nt1)
toCoreTerm h p (RepeatUntil2 nt1 args1 l r i s nt2 args2) =
    (Core.RepeatUntil nt1 args1 l r i l0 r0 nt2 args2, ref h (Core.End nt2))
  where (l0, r0) = toCoreStartingOn h p s

toCoreStartingOn :: ExpHelpers nt t id e -> e -> StartingOn e -> (e, e)
toCoreStartingOn h p StartingOn0 = (p, ref h Core.EOI)
toCoreStartingOn h p (StartingOn1 l) = (p, add h p l)
toCoreStartingOn _ _ (StartingOn2 l r) = (l, r)
