{-# LANGUAGE DeriveFunctor #-}
module FullIPG ( 
    Grammar(..), Rule(..), Alternative(..), Term(..),
    ExpHelpers(..),
    toCore, toCoreRule, toCoreAlternative, toCoreTerm,
) where
import qualified CoreIPG as Core

-- TODO: where clauses
newtype Grammar nt t id e = Grammar [Rule nt t id e]
    deriving ( Functor, Show )

data Rule nt t id e = Rule nt [Alternative nt t id e] -- A -> alt_1 / ... / alt_n
    deriving ( Functor, Show )

newtype Alternative nt t id e = Alternative [Term nt t id e] -- tm_1 ... tm_n
    deriving ( Functor, Show )

data Term nt t id e 
    = NonTerminal0 nt     -- A
    | NonTerminal1 nt e   -- A[e_l]
    | NonTerminal2 nt e e -- A[e_l, e_r]
    | Terminal0 t         -- s
    | Terminal1 t e       -- s[e_l]
    | Terminal2 t e e     -- s[e_l, e_r]
    | id := e             -- {id = e}
    | Guard e             -- ?[e]
    | Array id e e nt e e -- for id=e_1 to e_2 do A[e_l, e_r]
    | Any0 id             -- {id = .}
    | Any1 id e           -- {id = .[e]}
    | Slice0 id           -- {id = *}
    | Slice1 id e         -- {id = *[e_l]}
    | Slice2 id e e       -- {id = *[e_l, e_r]}
    -- TODO: Switch
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
toCoreRule h (Rule nt alts) = Core.Rule nt (map (toCoreAlternative h) alts)

toCoreAlternative :: ExpHelpers nt t id e -> Alternative nt t id e -> Core.Alternative nt t id e
toCoreAlternative h (Alternative terms) = Core.Alternative (go terms (num h 0) [])
    where go [] _ acc = reverse acc
          go (t:ts) nt acc = let (t', nt') = toCoreTerm h nt t
                             in go ts nt' (t':acc)

toCoreTerm :: ExpHelpers nt t id e -> e -> Term nt t id e -> (Core.Term nt t id e, e)
toCoreTerm h p (NonTerminal0 nt) = (Core.NonTerminal nt p (ref h Core.EOI), ref h (Core.End nt))
toCoreTerm h p (NonTerminal1 nt l) = (Core.NonTerminal nt p (add h p l), ref h (Core.End nt))
toCoreTerm h _ (NonTerminal2 nt l r) = (Core.NonTerminal nt l r, ref h (Core.End nt))
toCoreTerm h p (Terminal0 t) = (Core.Terminal t p (add h p (len h t)), add h p (len h t))
toCoreTerm h p (Terminal1 t l) = (Core.Terminal t p (add h p l), add h p (len h t))
toCoreTerm h _ (Terminal2 t l r) = (Core.Terminal t l r, add h l (len h t))
toCoreTerm _ p (i := e) = (i Core.:= e, p)
toCoreTerm _ p (Guard e) = (Core.Guard e, p)
toCoreTerm _ _ (Array i s e nt l r) = (Core.Array i s e nt l r, error "Can't infer after Array") -- TODO
toCoreTerm h p (Any0 i) = (Core.Any i p, add h p (num h 1))
toCoreTerm h _ (Any1 i l) = (Core.Any i l, add h l (num h 1))
toCoreTerm h p (Slice0 i) = (Core.Slice i p (ref h Core.EOI), ref h Core.EOI)
toCoreTerm h p (Slice1 i l) = (Core.Slice i p (add h p l), (add h p l))
toCoreTerm _ _ (Slice2 i l r) = (Core.Slice i l r, r)
