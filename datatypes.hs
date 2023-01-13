module Datatypes where

data LetterNumberSequence
  = EmptyLNS
  | Cons Char LetterNumberSequence

data Identifier = MakeId Char LetterNumberSequence

-- todo assert first letter is capital
data Variable = MakeVar Char LetterNumberSequence

type Constant = Identifier

type TermSequence = Sequence Term
type AtomSequence = Sequence Atom

data Sequence a
  = EndSequence a
  | MakeSequence a (Sequence a)

instance Functor Sequence where
  fmap f (EndSequence end) = EndSequence (f end)
  fmap f (MakeSequence el seq) = MakeSequence (f el) (fmap f seq)

data Term
  = MakeTermC Constant
  | MakeTermV Variable
  | MakeTermAtom Atom

data Atom = MakeAtom Identifier TermSequence

type Fact = Atom

data Rule = MakeRule Atom AtomSequence

type Database = ([Rule], [Fact])

data QueryResult
  = EndQR Bool
  | MakeQR (Variable, Replacement) QueryResult

data Replacement
  = ReplaceId Identifier
  | ReplaceVar Variable

data ResolutionTree
  = EmptyRT
  | NodeRT [ResolutionTree]
  | LeafRT QueryResult