{-# LANGUAGE DeriveFunctor #-}
module CoreIPG ( 
    Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..),
    nonTerminals, nonArrayNonTerminals,
) where
import Data.List ( nub ) -- base

newtype Grammar nt t id e = Grammar [Rule nt t id e]
    deriving ( Functor, Show )

  -- A{a_1, ..., a_m} -> alt_1 / ... / alt_n;
data Rule nt t id e = Rule nt [id] [Alternative nt t id e]
    deriving ( Functor, Show )

-- tm_1 ... tm_n where { grammar }
data Alternative nt t id e = Alternative [Term nt t id e] (Maybe (Grammar nt t id e))
    deriving ( Functor, Show )

data Term nt t id e 
    = NonTerminal nt [e] e e    -- A(a_1, ..., a_m)[e_l, e_r]
    | Terminal t e e            -- s[e_l, e_r]
    | id := e                   -- {id = e}
    | Guard e                   -- ?[e]
    | Array id e e nt [e] e e   -- for id=e_1 to e_2 do A(a_1, ..., a_m)[e_l, e_r]
    | Any id e                  -- {id = .[e]}
    | Slice id e e              -- {id = *[l, r]}
    | Repeat nt [e] id nt [e]   -- repeat A(a_1, ..., a_m).id until B(b_1, ..., b_k)
  -- TODO: Add Empty which succeeds only if the input is empty. So that we can do repeat until Empty.
  deriving ( Functor, Show )
    
data Ref nt id e
    = Id id               -- id, essentially self.id
    | Attr nt id          -- A.id
    | Index nt e id       -- A(e).id
    | EOI                 -- EOI
    | Start nt            -- A.start
    | End nt              -- A.end
  deriving ( Functor, Show )

nonArrayNonTerminals :: (Eq nt) => [Term nt t id e] -> [nt]
nonArrayNonTerminals = nub . concatMap processTerm
    where processTerm (NonTerminal nt _ _ _) = [nt]
          processTerm (Repeat nt1 _ _ nt2 _) = [nt1, nt2]
          processTerm _ = []

nonTerminals :: (Eq nt) => [Term nt t id e] -> [nt]
nonTerminals = nub . concatMap processTerm
    where processTerm (NonTerminal nt _ _ _) = [nt]
          processTerm (Repeat nt1 _ _ nt2 _) = [nt1, nt2]
          processTerm (Array _ _ _ nt _ _ _) = [nt]
          processTerm _ = []

-- TODO: Add pretty-printer.
-- TODO: Add validate to check basic syntactic properties, e.g. the non-terminals referenced in
-- terms are non-terminals that occur earlier in the alternative, etc.
-- Also, perhaps add termination checker.
