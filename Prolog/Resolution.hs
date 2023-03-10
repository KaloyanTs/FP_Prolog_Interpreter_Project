module Prolog.Resolution where

import Prolog.Checkers
import Prolog.Conversions
import Prolog.Datatypes
import Prolog.Unification
import Tools

uniqueQRs :: [QueryResult] -> [QueryResult]
uniqueQRs [] = []
uniqueQRs (qr : qrs) = qr : filter (/= qr) qrs

buildRTree :: Database -> [Atom] -> QueryResult -> ResolutionTree
buildRTree _ [] qr = LeafRT qr
buildRTree db@(r, f) arr@(a : as) qr = NodeRT children
  where
    children = factChildren ++ ruleChildren
    factChildren = map (\fqr -> buildRTree db (map (`apply` fqr) as) (appendQR fqr qr)) (fst u)
    ruleChildren = map (\(al, rqr) -> buildRTree db (map (`apply` rqr) (al ++ as)) (appendQR rqr qr)) (snd u)
    u = unifiers db a

unifiers :: Database -> Atom -> ([QueryResult], [([Atom], QueryResult)])
unifiers db@(r, f) a =
  ( filter notBad (map (\fact -> toBeUnified (factToTerm fact, MakeTermAtom a)) f),
    filter (notBad . snd) (map (\(MakeRule ah as) -> (asToAtomArray as, toBeUnified (MakeTermAtom ah, MakeTermAtom a))) r)
  )

collectSolutions :: ResolutionTree -> [QueryResult]
collectSolutions EmptyRT = []
collectSolutions (LeafRT qr) = [qr]
collectSolutions (NodeRT ts) = concatMap collectSolutions ts

needed :: [Atom] -> [QueryResult] -> [QueryResult]
needed a qrs = filter notBad (map (onlyUseful vars) qrs)
  where
    vars = uniques $ concatMap getVariablesAtom a
    onlyUseful :: [Variable] -> QueryResult -> QueryResult
    onlyUseful [] _ = EndQR True
    onlyUseful arr (EndQR b) = EndQR b
    onlyUseful arr (MakeQR qr@(var, ReplaceId _) qrs)
      | var `elem` arr = MakeQR qr (onlyUseful (filter (/= var) arr) qrs)
      | otherwise = onlyUseful (filter (/= var) arr) qrs
    onlyUseful arr (MakeQR qr@(var, ReplaceVar var2) qrs)
      | var2 `elem` arr = MakeQR (var2, ReplaceVar var) (onlyUseful (filter (/= var2) arr) qrs)
      | var `elem` arr = MakeQR qr (onlyUseful (filter (/= var) arr) qrs)
      | otherwise = onlyUseful (filter (/= var) arr) qrs

resolve :: [Fact] -> Database -> [QueryResult]
resolve a db =  needed a $ collectSolutions $ buildRTree db a (EndQR True)

interpreteInput :: String -> Database -> [QueryResult]
interpreteInput input db@(r, f)
  | isFact input = resolve [toAtom (init input) False] db
  | isEquality input = [toBeUnified (toEquality input)]
  | otherwise = resolve (map (`toAtom` False) (splitBy ',' (init input))) db