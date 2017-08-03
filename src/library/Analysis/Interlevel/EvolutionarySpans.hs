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

import           Control.Monad
import           Control.Monad.List

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.DiagramAlgorithms
import           Base.Valid
import           Category.TypedGraph                      ()
import           Category.TypedGraphRule
import           Rewriting.DPO.TypedGraphRule
import           Util.Monad

-- | All actual possible kinds of Evolutionary Spans, it indicates the
-- conflict existence on an overlap situation before and after evolution
type CPE = (Bool, Bool)

-- | Represents the two evolutionary matches, and the kind of span
data EvoSpan n e = EvoSpan {
  leftMatch  :: RuleMorphism n e,
  rightMatch :: RuleMorphism n e,
  cpe        :: CPE
  } deriving (Eq,Show)

-- | Given a list of second-order rules, calculate all Evolutionary Spans
-- This analysis is supposed to be symmetric, here is considering only this case
allEvolSpans :: [(String, SndOrderRule n e)] -> TGRuleCat n e [(String, String, [EvoSpan n e])]
-- combine rules symmetrically
allEvolSpans []           = return []
allEvolSpans rules@(r:rs) = (++) <$> mapM (evolSpans r) rules <*> allEvolSpans rs

-- combine rules asymmetrically
--allEvolSpans dpoConf sndOrderRules = concatMap (\r1 -> map (evolSpans dpoConf r1) sndOrderRules) sndOrderRules

-- | Gets all Evolutionary Spans of two Second Order Rules
evolSpans :: (String, SndOrderRule n e) -> (String, SndOrderRule n e) -> TGRuleCat n e (String, String, [EvoSpan n e])
evolSpans (n1,r1) (n2,r2) = do
  let

    -- filter to catch only interesting situations
    --filteredSpans = filter (\s -> cpe s `elem` [FolDuse, DuseFol]) spans

    r1Left = leftObject r1
    r2Left = leftObject r2
    r1Right = rightObject r1
    r2Right = rightObject r2

    leftR1 = buildProduction (mappingLeft (leftMorphism r1)) (mappingLeft (rightMorphism r1)) []
    leftR2 = buildProduction (mappingLeft (leftMorphism r2)) (mappingLeft (rightMorphism r2)) []

  spans <- runListT $ do
    (m1, m2) <- pickOne $ findJointlyEpicPairs (matchMorphism, leftR1) (matchMorphism, leftR2)
    guardM $ isValid (codomain m1)
    guardM . liftTGraph $
      satisfiesRewritingConditions r1Left (mappingLeft m1) `andM` satisfiesRewritingConditions r2Left (mappingLeft m2)
      `andM` satisfiesRewritingConditions r1Right (mappingLeft m1) `andM` satisfiesRewritingConditions r2Right (mappingLeft m2)
    lift (EvoSpan m1 m2 <$> classify r1 r2 (m1, m2))
  return (n1, n2, spans)

-- | Given two second-order rules and their matches overlaped, return their type
classify :: SndOrderRule n e -> SndOrderRule n e -> (RuleMorphism n e, RuleMorphism n e) -> TGRuleCat n e CPE
classify r1 r2 (m1,m2) = liftTGraph ((,) <$> deleteUseFlGl <*> deleteUseFlGl'')
  where
    isConflict l1 l2 m =
         isDeleteUse l1 m
      `orM` isProduceDangling l1 l2 m
      `orM` isProduceForbid l1 l2 m

    deleteUseFlGl =
      isConflict
        (leftObject r1)
        (leftObject r2)
        (mappingLeft m1, mappingLeft m2) `orM`
      isConflict
        (leftObject r2)
        (leftObject r1)
        (mappingLeft m2, mappingLeft m1)

    deleteUseFlGl'' =
      isConflict
        (rightObject r1)
        (rightObject r2)
        (mappingRight m1, mappingRight m2) `orM`
      isConflict
        (rightObject r2)
        (rightObject r1)
        (mappingRight m2, mappingRight m1)
