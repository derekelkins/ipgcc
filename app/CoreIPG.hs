module CoreIPG ( 
    Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..),
    nonTerminals, nonArrayNonTerminals,
) where

newtype Grammar nt t id e = Grammar [Rule nt t id e]
    deriving ( Show )

data Rule nt t id e = Rule nt [Alternative nt t id e] -- A -> alt_1 / ... / alt_n
    deriving ( Show )

newtype Alternative nt t id e = Alternative [Term nt t id e] -- tm_1 ... tm_n
    deriving ( Show )

data Term nt t id e 
    = NonTerminal nt e e  -- A[e_l, e_r]
    | Terminal t e e      -- s[e_l, e_r]
    | id := e             -- {id = e}
    | Guard e             -- <e>
    | Array id e e nt e e -- for id=e_1 to e_2 do A[e_l, e_r]
    | Any id e            -- {id = input[e]} -- TODO: Replace these with call expressions?
    | Slice id e e        -- {id = input[l, r]}
  deriving ( Show )
    
data Ref id nt e
    = Id id               -- id, essentially self.id
    | Attr nt id          -- A.id
    | Index nt e id       -- A(e).id
    | EOI                 -- EOI
    | Start nt            -- A.start
    | End nt              -- A.end
  deriving ( Show )

nonArrayNonTerminals :: [Term nt t id e] -> [nt]
nonArrayNonTerminals = concatMap processTerm
    where processTerm (NonTerminal nt _ _) = [nt]
          processTerm _ = []

nonTerminals :: [Term nt t id e] -> [nt]
nonTerminals = concatMap processTerm
    where processTerm (NonTerminal nt _ _) = [nt]
          processTerm (Array _ _ _ nt _ _) = [nt]
          processTerm _ = []

-- TODO: Add pretty-printer.
-- TODO: Add validate to check basic syntactic properties, e.g. the non-terminals referenced in
-- terms are non-terminals that occur earlier in the alternative, etc.
-- Also, perhaps add termination checker.
