{-|
Module      : EvolutionarySpans
Description : Implements the evolutionary spans of match overlaps inter-level conflict.
Stability   : development
-}

module Analysis.Interlevel.EvolutionarySpans
  ( allEvolSpans
  , EvoSpan(..)
  , leftMatch
  , rightMatch
  , cpe
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Valid
import           Analysis.DiagramAlgorithms
import           SndOrder.Morphism
import           SndOrder.Rule

-- | All actual possible kinds of Evolutionary Spans,
-- it indicates the overlap situation before and after evolution
data CPE = FolFol | DuseDuse | FolDuse | DuseFol deriving(Eq,Show)

-- | Represents the two evolutionary matches, and the kind of span
data EvoSpan a b = EvoSpan {
  leftMatch  :: RuleMorphism a b,
  rightMatch :: RuleMorphism a b,
  cpe :: CPE
  } deriving (Eq,Show)

-- | Given a list of second order rules, calculate all Evolutionary Spans
-- (FIX: check if is needed to calculate symetric cases)
allEvolSpans :: DPOConfig -> [(String, SndOrderRule a b)] -> [(String, [EvoSpan a b])]
allEvolSpans dpoConf sndOrderRules = concatMap (\r1 -> map (evolSpans dpoConf r1) sndOrderRules) sndOrderRules

-- | Gets all Evolutionary Spans of two Second Order Rules
evolSpans :: DPOConfig -> (String, SndOrderRule a b) -> (String, SndOrderRule a b) -> (String, [EvoSpan a b])
evolSpans config (n1,r1) (n2,r2) = (ruleNames, spans)
  where
    ruleNames = n1 ++ " (evoSpan) " ++ n2
    spans = map (\m@(m1,m2) -> EvoSpan m1 m2 (classify config r1 r2 m)) xs''
    
    -- filter to catch only interesting situations
    --filteredSpans = filter (\s -> cpe s `elem` [FolDuse, DuseFol]) spans
    
    r1Left = codomain (left r1)
    r2Left = codomain (left r2)
    r1Right = codomain (right r1)
    r2Right = codomain (right r2)

    leftR1 = production (mappingLeft (left r1)) (mappingLeft (right r1)) []
    leftR2 = production (mappingLeft (left r2)) (mappingLeft (right r2)) []

    pairs = createPairs (matchRestriction config == MonoMatches) leftR1 leftR2

    xs = filter (\(m1,_) -> valid (codomain m1)) pairs
    xs' = filter (\(m1,m2) -> satsGluingNacsBoth config (r1Left, mappingLeft m1) (r2Left, mappingLeft m2)) xs
    xs'' = filter (\(m1,m2) -> satsGluingNacsBoth config (r1Right, mappingLeft m1) (r2Right, mappingLeft m2)) xs'

-- | Given two second order rules and their matches overlaped, return their type
classify :: DPOConfig -> SndOrderRule a b -> SndOrderRule a b -> (RuleMorphism a b, RuleMorphism a b) -> CPE
classify config r1 r2 (m1,m2) =
  case (deleteUseFlGl, deleteUseFlGl'') of
    (True,True) -> DuseDuse
    (True,False) -> DuseFol
    (False,True) -> FolDuse
    (False,False) -> FolFol
  where
    deleteUseFlGl =
      deleteUse config
        (codomain (left r1))
        (mappingLeft m1, mappingLeft m2) ||
      deleteUse config
        (codomain (left r2))
        (mappingLeft m2, mappingLeft m1)
    
    deleteUseFlGl'' =
      deleteUse config
        (codomain (right r1))
        (mappingRight m1, mappingRight m2) ||
      deleteUse config
        (codomain (right r2))
        (mappingRight m2, mappingRight m1)
