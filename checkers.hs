module Checkers where

import Data.Char
import Datatypes
import Tools

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (x : xs) =
  isAsciiLower x
    && all (\y -> isLetter y || isDigit y || y == '_') xs

isVariable :: String -> Bool
isVariable [] = False
isVariable (x : xs) =
  isAsciiUpper x
    && all (\y -> isLetter y || isDigit y) xs

isConstant :: String -> Bool
isConstant = isIdentifier

isAtom :: String -> Bool
isAtom l =
  isValidPar && isIdentifier beforePar && not (null insidePar)
    && all (isTerm . removeEndSpace) (splitBy ',' insidePar)
  where
    beforePar = takeWhile (/= '(') l
    parPart = dropWhile (/= '(') l
    isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
    insidePar = init $ tail parPart

isTerm :: String -> Bool
isTerm [] = False
isTerm l@(x : xs) =
  isConstant l
    || isVariable l
    || isAtom l

isFact :: String -> Bool
isFact l =
  (not . null) l
    && last l == '.'
    && (isAtom . init) l

isRule :: String -> Bool
isRule l =
  (not . null) l
    && last l == '.'
    && hasSpecial
    && isAtom beforeSpecial
    && all
      isAtom
      (splitBy ',' afterSpecial)
  where
    noDot = init l
    hasSpecial = " :- " `isSubstring` noDot
    beforeSpecial = takeWhile (/= ' ') noDot
    afterSpecial = drop 4 (dropWhile (/= ' ') noDot)

isEquality :: String -> Bool
isEquality str =
  '=' `elem` str
    && last str == '.'
    && isTerm before
    && isTerm after
  where
    noDot = init str
    breaking = break (== '=') noDot
    before =
      reverse $
        dropWhile (== ' ') $
          reverse $
            fst breaking
    after = dropWhile (== ' ') $ tail $ snd breaking

isComment :: String -> Bool
isComment l = (not . null) l && head (dropWhile (== ' ') l) == '%'

termContainsVariable :: Term -> Bool
termContainsVariable (MakeTermC _) = False
termContainsVariable (MakeTermV _) = True
termContainsVariable (MakeTermAtom a) = atomContainsVariable a

atomContainsVariable :: Atom -> Bool
atomContainsVariable (MakeAtom _ ts) = tsContainsVariable ts

tsContainsVariable :: TermSequence -> Bool
tsContainsVariable (EndSequence t) = termContainsVariable t
tsContainsVariable (MakeSequence t ts) =
  termContainsVariable t
    || tsContainsVariable ts

anyAS :: (Atom->Bool) -> AtomSequence -> Bool
anyAS p (EndSequence a) = p a
anyAS p (MakeSequence a as) = p a || anyAS p as

allAS :: (Atom->Bool) -> AtomSequence -> Bool
allAS p (EndSequence a) = p a
allAS p (MakeSequence a as) = p a && allAS p as

good :: QueryResult -> Bool
good (EndQR True) = True
good _ = False

bad :: QueryResult -> Bool
bad (EndQR False) = True
bad _ = False

notBad :: QueryResult -> Bool
notBad = not.bad

emptyQR :: QueryResult -> Bool
emptyQR (EndQR _) = True
emptyQR _ = False

consult :: String -> (Bool, [String])
consult contents = (truth, if truth then [] else filter (\x -> not (isFact x || isRule x || isComment x)) (extractData contents))
  where
    truth = all (\x -> isFact x || isRule x || isComment x) (extractData contents)