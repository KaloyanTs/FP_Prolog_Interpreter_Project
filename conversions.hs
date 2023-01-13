module Conversions where

import Checkers
import Datatypes
import Tools

toLNS :: String -> LetterNumberSequence
toLNS = foldr Cons EmptyLNS

toIdentifier :: String -> Identifier
toIdentifier [] = error "empty String cannot be an identifier"
toIdentifier s@(x : xs)
  | not (isIdentifier s) = error $ s ++ " cannot be a valid identifier"
  | otherwise = MakeId x (toLNS xs)

toVariable :: String -> Bool -> Variable
toVariable [] _ = error "empty String cannot be a variable"
toVariable s@(x : xs) file
  | not (isVariable s) = error $ s ++ " cannot be a valid variable"
  | otherwise = MakeVar x (toLNS (if file then xs ++ "0" else xs))

toConstant :: String -> Constant
toConstant [] = error "empty String cannot be a constant"
toConstant s@(x : xs)
  | not (isConstant s) = error $ s ++ " cannot be a valid constant"
  | otherwise = MakeId x (toLNS xs)

toAtom :: String -> Bool -> Atom
toAtom l file
  | not (isAtom l) = error $ l ++ " cannot be an atom"
  | otherwise = MakeAtom id (toTermSequence terms)
  where
    id = toIdentifier (takeWhile (/= '(') l)
    parPart = dropWhile (/= '(') l
    isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
    terms = map ((`toTerm` file) . removeEndSpace) (splitBy ',' (init (tail parPart)))

toTerm :: String -> Bool -> Term
toTerm l file
  | not (isTerm l) = error $ l ++ " cannot be a term"
  | isConstant l = MakeTermC (toConstant l)
  | isVariable l = MakeTermV (toVariable l file)
  | otherwise = MakeTermAtom (toAtom l file)

toTermSequence :: [Term] -> TermSequence
toTermSequence [] = error "term sequence has at least one term"
toTermSequence [t] = EndSequence t
toTermSequence (x : xs) = MakeSequence x (toTermSequence xs)

toFact :: String -> Bool -> Fact
toFact s file
  | not (isFact s) = error $ s ++ " cannot be a valid fact"
  | otherwise = toAtom (init s) file

toRule :: String -> Bool -> Rule
toRule l file
  | not (isRule l) = error $ l ++ "cannot be a rule"
  | otherwise = MakeRule (toAtom beforeSpecial file) (toAtomSequence atoms)
  where
    noDot = init l
    breaking = break (== ':') noDot
    beforeSpecial = init $ fst breaking
    afterSpecial = drop 3 $ snd breaking
    atoms = map (\s -> toAtom (removeEndSpace s) file) (splitBy ',' afterSpecial)

toEquality :: String -> (Term, Term)
toEquality str
  | not (isEquality str) = error $ str ++ " cannot be an equality"
  | otherwise = (toTerm before False, toTerm after False)
  where
    noDot = init str
    before =
      reverse $
        dropWhile (== ' ') $
          reverse $
            takeWhile (/= '=') noDot
    after = dropWhile (== ' ') $ tail $ dropWhile (/= '=') noDot

toAtomSequence :: [Atom] -> AtomSequence
toAtomSequence [] = error "atom sequence consists of at least one atom"
toAtomSequence [a] = EndSequence a
toAtomSequence (x : xs) = MakeSequence x (toAtomSequence xs)

showAtom :: Atom -> String
showAtom (MakeAtom id ts) = showIdentifier id ++ "(" ++ showTermSequence ts ++ ")"

showTermSequence :: TermSequence -> String
showTermSequence (EndSequence t) = showTerm t
showTermSequence (MakeSequence t ts) = showTerm t ++ "," ++ showTermSequence ts

showTerm :: Term -> String
showTerm (MakeTermC c) = showConstant c
showTerm (MakeTermV v) = showVariable v
showTerm (MakeTermAtom a) = showAtom a

showConstant :: Identifier -> String
showConstant = showIdentifier

showVariable :: Variable -> String
showVariable (MakeVar c lns) = c : showLNS lns

showIdentifier :: Identifier -> String
showIdentifier (MakeId c lns) = c : showLNS lns

showReplacement :: Replacement -> String
showReplacement (ReplaceId id) = showIdentifier id
showReplacement (ReplaceVar var) = showVariable var

showLNS :: LetterNumberSequence -> String
showLNS EmptyLNS = []
showLNS (Cons a lns) = a : showLNS lns

showRule :: Rule -> String
showRule (MakeRule a as) = showAtom a ++ " :- " ++ showAtomSequence as ++ "."

showAtomSequence :: AtomSequence -> String
showAtomSequence (EndSequence a) = showAtom a
showAtomSequence (MakeSequence a as) = showAtom a ++ "," ++ showAtomSequence as

showFact :: Atom -> String
showFact a = showAtom a ++ "."

showFacts :: Database -> [String]
showFacts (r, f) = map showFact f

showRules :: Database -> [String]
showRules (r, f) = map showRule r

factToTerm :: Fact -> Term
factToTerm f = MakeTermAtom (toAtom (init (showFact f)) False)

termToAtom :: Term -> Atom
termToAtom (MakeTermAtom a) = a
termToAtom _ = error "impossible conversion..."

tsToTermArray :: TermSequence -> [Term]
tsToTermArray (EndSequence t) = [t]
tsToTermArray (MakeSequence t ts) = t : tsToTermArray ts

asToAtomArray :: AtomSequence -> [Atom]
asToAtomArray (EndSequence a) = [a]
asToAtomArray (MakeSequence a as) = a : asToAtomArray as

getVariablesAtom :: Atom -> [Variable]
getVariablesAtom (MakeAtom _ ts) = getVariablesTS ts

getVariablesTS :: TermSequence -> [Variable]
getVariablesTS (EndSequence t) = getVariablesTerm t
getVariablesTS (MakeSequence t ts) = getVariablesTerm t ++ getVariablesTS ts

getVariablesTerm :: Term -> [Variable]
getVariablesTerm (MakeTermC _) = []
getVariablesTerm (MakeTermV v) = [v]
getVariablesTerm (MakeTermAtom a) = getVariablesAtom a

interpreteCode :: [String] -> Database
interpreteCode c = (rules, facts)
  where
    rules = map (`toRule` True) (filter isRule c)
    facts = map (`toFact` True) (filter isFact c)