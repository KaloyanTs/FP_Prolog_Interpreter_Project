module Identities
where

import Datatypes

-- todo solve using fmap and classes ???

-- areIdenticalIds :: Identifier -> Identifier -> Bool
-- areIdenticalIds (MakeId c1 lns1) (MakeId c2 lns2) =
--   c1 == c2
--     && lns1==lns2

-- areIdenticalConstants :: Constant -> Constant -> Bool
-- areIdenticalConstants = areIdenticalIds

-- -- areIdenticalVariables :: Variable -> Variable -> Bool
-- -- areIdenticalVariables (MakeVar c1 lns1) (MakeVar c2 lns2) =
-- --   c1 == c2
-- --     && lns1==lns2

-- areIdenticalAtoms :: Atom -> Atom -> Bool
-- areIdenticalAtoms (MakeAtom id1 ts1) (MakeAtom id2 ts2) = id1==id2 && areIdenticalTS ts1 ts2

-- areIdenticalFacts :: Fact -> Fact -> Bool
-- areIdenticalFacts = areIdenticalAtoms

-- areIdenticalTerms :: Term -> Term -> Bool
-- areIdenticalTerms (MakeTermC c1) (MakeTermC c2) = c1==c2
-- areIdenticalTerms (MakeTermV c1) (MakeTermV c2) = (==) c1 c2
-- areIdenticalTerms (MakeTermAtom c1) (MakeTermAtom c2) = areIdenticalAtoms c1 c2
-- areIdenticalTerms _ _ = False

-- areIdenticalTS :: TermSequence -> TermSequence -> Bool
-- areIdenticalTS (EndSequence t1) (EndSequence t2) = areIdenticalTerms t1 t2
-- areIdenticalTS (MakeSequence t1 ts1) (MakeSequence t2 ts2) =
--   areIdenticalTerms t1 t2
--     && areIdenticalTS ts1 ts2
-- areIdenticalTS _ _ = False