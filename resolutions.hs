module Resolutions where

import Checkers
import Conversions
import Datatypes
import Identities
import Unification

uniqueQRs :: [QueryResult] -> [QueryResult]
uniqueQRs [] = []
uniqueQRs (qr : qrs) = qr : filter (not . areIdenticalQR qr) qrs

buildRTree :: Database -> [Atom] -> QueryResult -> ResolutionTree
buildRTree _ [] qr = LeafRT qr
buildRTree db@(r, f) arr@(a : as) qr = NodeRT children
  where
    children = factChildren ++ ruleChildren
    factChildren = map (\fqr -> buildRTree db (map (`apply` qr) as) (appendQR fqr qr)) (fst u)
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

needed :: Atom -> [QueryResult] -> [QueryResult]
needed a qrs = filter notBad (map (onlyUseful vars) qrs)
  where
    vars = getVariablesAtom a
    onlyUseful :: [Variable] -> QueryResult -> QueryResult
    -- onlyUseful _ qr = qr
    onlyUseful [] _ = EndQR True
    onlyUseful arr (EndQR b) = EndQR b
    onlyUseful arr (MakeQR qr@(var, _) qrs)
      | any (areIdenticalVariables var) arr = MakeQR qr (onlyUseful (filter (not . areIdenticalVariables var) arr) qrs)
      | otherwise = onlyUseful (filter (not . areIdenticalVariables var) arr) qrs

interpreteInput :: String -> Database -> [QueryResult]
interpreteInput input db@(r, f)
  | isFact input = needed readyAtom $ collectSolutions $ buildRTree db [readyAtom] (EndQR True)
  | otherwise = [toBeUnified (toEquality input)]
  where
    readyAtom = toAtom (init input)