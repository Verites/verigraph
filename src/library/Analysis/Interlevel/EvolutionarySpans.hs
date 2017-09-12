{-|
Module      : EvolutionarySpans
Description : Implements the evolutionary spans of match overlaps inter-level conflict.
Stability   : development
-}

module Analysis.Interlevel.EvolutionarySpans
  ( allEvolSpans
  , EvoSpan()
  , leftMatch
  , rightMatch
  , cpe
  ) where

import           Abstract.Category
import           Abstract.Category.Finitary
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Base.Valid
import           Category.TypedGraphRule
import           Rewriting.DPO.TypedGraphRule

-- | All actual possible kinds of Evolutionary Spans, it indicates the
-- conflict existence on an overlap situation before and after evolution
type CPE = (Bool, Bool)

-- | Represents the two evolutionary matches, and the kind of span
data EvoSpan a b = EvoSpan {
  leftMatch  :: RuleMorphism a b,
  rightMatch :: RuleMorphism a b,
  cpe        :: CPE
  } deriving (Eq,Show)

-- | Given a list of second-order rules, calculate all Evolutionary Spans
-- This analysis is supposed to be symmetric, here is considering only this case
allEvolSpans :: MorphismsConfig (RuleMorphism a b) -> [(String, SndOrderRule a b)] -> [(String, String, [EvoSpan a b])]
-- combine rules symmetrically
allEvolSpans _ []                 = []
allEvolSpans dpoConf rules@(r:rs) = map (evolSpans dpoConf r) rules ++ allEvolSpans dpoConf rs

-- combine rules asymmetrically
--allEvolSpans dpoConf sndOrderRules = concatMap (\r1 -> map (evolSpans dpoConf r1) sndOrderRules) sndOrderRules

-- | Gets all Evolutionary Spans of two Second Order Rules
evolSpans :: MorphismsConfig (RuleMorphism a b) -> (String, SndOrderRule a b) -> (String, SndOrderRule a b) -> (String, String, [EvoSpan a b])
evolSpans conf' (n1,r1) (n2,r2) = (n1, n2, spans)
  where
    spans = map (\m@(m1,m2) -> EvoSpan m1 m2 (classify conf' r1 r2 m)) xs''

    -- filter to catch only interesting situations
    --filteredSpans = filter (\s -> cpe s `elem` [FolDuse, DuseFol]) spans

    r1Left = leftObject r1
    r2Left = leftObject r2
    r1Right = rightObject r1
    r2Right = rightObject r2

    leftR1 = Production (mappingLeft (leftMorphism r1)) (mappingLeft (rightMorphism r1)) []
    leftR2 = Production (mappingLeft (leftMorphism r2)) (mappingLeft (rightMorphism r2)) []

    pairs = findJointSurjections (matchRestriction conf', leftR1) (matchRestriction conf', leftR2)

    conf = toFstOrderMorphismsConfig conf'
    xs = filter (\(m1,_) -> isValid (codomain m1)) pairs
    xs' = filter (\(m1,m2) -> satisfiesRewritingConditions conf r1Left (mappingLeft m1) && satisfiesRewritingConditions conf r2Left (mappingLeft m2)) xs
    xs'' = filter (\(m1,m2) -> satisfiesRewritingConditions conf r1Right (mappingLeft m1) && satisfiesRewritingConditions conf r2Right (mappingLeft m2)) xs'

-- | Given two second-order rules and their matches overlaped, return their type
classify :: MorphismsConfig (RuleMorphism a b) -> SndOrderRule a b -> SndOrderRule a b -> (RuleMorphism a b, RuleMorphism a b) -> CPE
classify conf' r1 r2 (m1,m2) = (deleteUseFlGl, deleteUseFlGl'')
  where
    isConflict c l1 l2 m =
         isDeleteUse c l1 m
      || isProduceDangling c l1 l2 m
      || isProduceForbid c l1 l2 m

    conf = toFstOrderMorphismsConfig conf'

    deleteUseFlGl =
      isConflict conf (leftObject r1) (leftObject r2) (mappingLeft m1, mappingLeft m2) ||
      isConflict conf (leftObject r2) (leftObject r1) (mappingLeft m2, mappingLeft m1)

    deleteUseFlGl'' =
      isConflict conf (rightObject r1) (rightObject r2) (mappingRight m1, mappingRight m2) ||
      isConflict conf (rightObject r2) (rightObject r1) (mappingRight m2, mappingRight m1)
