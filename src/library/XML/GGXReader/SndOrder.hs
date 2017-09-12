module XML.GGXReader.SndOrder (instantiateSndOrderRules) where

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Category.TypedGraphRule
import qualified Data.Graphs                        as G
import           Data.Graphs.Morphism               as GM
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           XML.GGXReader.Span
import           XML.ParsedTypes
import qualified XML.ParseSndOrderRule              as SO
import           XML.Utilities

instantiateSndOrderRules :: G.Graph (Maybe a) (Maybe b) -> [RuleWithNacs] -> [(String, Production (RuleMorphism a b))]
instantiateSndOrderRules typeGraph sndOrdRules = zip sndOrderNames d
  where
    a = SO.parseSndOrderRules sndOrdRules
    c = map (instantiateSndOrderRule typeGraph) a
    d = map (\(_,(l,r),n) -> Production l r n) c
    sndOrderNames = map fstOfThree c

instantiateSndOrderRule :: G.Graph (Maybe a) (Maybe b) -> (SndOrderRuleSide, SndOrderRuleSide,[SndOrderRuleSide]) -> (String,(RuleMorphism a b, RuleMorphism a b),[RuleMorphism a b])
instantiateSndOrderRule typegraph (l@(_,nameL,leftL),r@(_,_,rightR), n) = (nameL, instantiateMorphs, nacs)
  where
    ruleLeft = instantiateRule typegraph leftL
    ruleRight = instantiateRule typegraph rightR
    instantiateMorphs = instantiateRuleMorphisms (l,ruleLeft) (r,ruleRight)
    nacsRules = map (instantiateRule typegraph . (\(_,_,(x,_)) -> (x,[]))) n
    nacs = map (instantiateSndOrderNac (l,ruleLeft)) (zip n nacsRules)

instantiateSndOrderNac :: (SndOrderRuleSide, TypedGraphRule a b) -> (SndOrderRuleSide, TypedGraphRule a b) -> RuleMorphism a b
instantiateSndOrderNac (parsedLeft, l) (n, nacRule) = ruleMorphism l nacRule nacL nacK nacR
  where
    mapL = SO.getLeftObjNameMapping parsedLeft n
    mapR = SO.getRightObjNameMapping parsedLeft n
    nacL = instantiateNacMorphisms (leftObject l) (leftObject nacRule) mapL
    nacK = instantiateNacMorphisms (interfaceObject l) (interfaceObject nacRule) mapL
    nacR = instantiateNacMorphisms (rightObject l) (rightObject nacRule) mapR

instantiateNacMorphisms :: TypedGraph a b -> TypedGraph a b
                        -> [Mapping] -> TypedGraphMorphism a b
instantiateNacMorphisms graphL graphN mapping = buildTypedGraphMorphism graphL graphN maps
  where
    mapElements = map (\(x,_,y) -> (read y :: Int, read x :: Int)) mapping
    maps = buildGraphMorphism
             (domain graphL)
             (domain graphN)
             mapElements
             mapElements

instantiateRuleMorphisms :: (SndOrderRuleSide, TypedGraphRule a b)
                         -> (SndOrderRuleSide, TypedGraphRule a b)
                         -> (RuleMorphism a b , RuleMorphism a b)
instantiateRuleMorphisms (parsedLeft, l) (parsedRight, r) =
  (ruleMorphism ruleK l leftKtoLeftL interfaceKtoL rightKtoRightL,
   ruleMorphism ruleK r leftKtoLeftR interfaceKtoR rightKtoRightR)
    where
      graphKRuleL = interfaceObject l
      graphKRuleR = interfaceObject r
      graphLRuleL = leftObject l
      graphLRuleR = leftObject r
      graphRRuleL = rightObject l
      graphRRuleR = rightObject r

      mappingBetweenLeft = SO.getLeftObjNameMapping parsedLeft parsedRight
      mappingBetweenRight = SO.getRightObjNameMapping parsedLeft parsedRight

      ruleK = Production leftK rightK []

      graphLRuleK = domain leftKtoLeftL
      graphRRuleK = domain rightKtoRightL

      (leftKtoLeftL, leftKtoLeftR) =
        instantiateSpan graphLRuleL graphLRuleR mappingBetweenLeft

      (interfaceKtoL, interfaceKtoR) =
        instantiateSpan graphKRuleL graphKRuleR mappingBetweenLeft

      (rightKtoRightL, rightKtoRightR) =
        instantiateSpan graphRRuleL graphRRuleR mappingBetweenRight

      maps (_,_,((_,_,_,x),_)) = x
      (leftK, rightK) = instantiateSpan graphLRuleK graphRRuleK (maps parsedLeft)
