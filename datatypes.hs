module Datatypes where

data LetterNumberSequence
  = EmptyLNS
  | Cons Char LetterNumberSequence
  deriving Eq

data Identifier = MakeId Char LetterNumberSequence
  deriving Eq

-- asserting first letter is capital
data Variable = MakeVar Char LetterNumberSequence
  deriving Eq

type Constant = Identifier

type TermSequence = Sequence Term

type AtomSequence = Sequence Atom

data Sequence a
  = EndSequence a
  | MakeSequence a (Sequence a)
  deriving Eq

instance Functor Sequence where
  fmap f (EndSequence end) = EndSequence (f end)
  fmap f (MakeSequence el seq) = MakeSequence (f el) (fmap f seq)

data Term
  = MakeTermC Constant
  | MakeTermV Variable
  | MakeTermAtom Atom
  deriving Eq

data Atom = MakeAtom Identifier TermSequence
  deriving Eq

type Fact = Atom

data Rule = MakeRule Atom AtomSequence
  deriving Eq

type Database = ([Rule], [Fact])

data QueryResult
  = EndQR Bool
  | MakeQR (Variable, Replacement) QueryResult
  deriving Eq

data Replacement
  = ReplaceId Identifier
  | ReplaceVar Variable
  deriving Eq

data ResolutionTree
  = EmptyRT
  | NodeRT [ResolutionTree]
  | LeafRT QueryResult