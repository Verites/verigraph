module XML.GGXSndOrderReader (instantiateSndOrderRules) where

import           Abstract.DPO
import           Abstract.Morphism
import           Graph.GraphMorphism      as GM
import           Graph.GraphRule          as GR
import           Graph.RuleMorphism
import           Graph.SndOrderRule       ()
import           Graph.TypedGraphMorphism
import {-# SOURCE #-} XML.GGXReader
import           XML.ParsedTypes
import qualified XML.ParseSndOrderRule    as SO
import           XML.Utilities

instantiateSndOrderRules :: ParsedTypeGraph -> [RuleWithNacs] -> [(String, Production (RuleMorphism a b))]
instantiateSndOrderRules parsedTypeGraph sndOrdRules = zip sndOrderNames d
  where
    a = SO.parseSndOrderRules sndOrdRules
    c = map (instantiateSndOrderRule parsedTypeGraph) a
    d = map (\(_,(l,r),n) -> production l r n) c
    sndOrderNames = map fstOfThree c

instantiateSndOrderRule :: ParsedTypeGraph -> (SndOrderRuleSide, SndOrderRuleSide,[SndOrderRuleSide]) -> (String,(RuleMorphism a b, RuleMorphism a b),[RuleMorphism a b])
instantiateSndOrderRule typegraph (l@(_,nameL,leftL),r@(_,_,rightR), n) = (nameL, instantiateMorphs, nacs)
  where
    ruleLeft = instantiateRule typegraph (leftL,[])
    ruleRight = instantiateRule typegraph (rightR,[])
    instantiateMorphs = instantiateRuleMorphisms (l,ruleLeft) (r,ruleRight)
    nacsRules = map (instantiateRule typegraph) (map (\(_,_,x) -> (x,[])) n)
    nacs = map (instantiateSndOrderNac (l,ruleLeft)) (zip n nacsRules)

instantiateSndOrderNac :: (SndOrderRuleSide, GraphRule a b) -> (SndOrderRuleSide, GraphRule a b) -> RuleMorphism a b
instantiateSndOrderNac (parsedLeft, l) (n, nacRule) = ruleMorphism l nacRule nacL nacK nacR
  where
    mapL = SO.getLeftObjNameMapping parsedLeft n
    mapR = SO.getRightObjNameMapping parsedLeft n
    nacL = instantiateNacMorphisms (codomain (left l)) (codomain (left nacRule)) mapL
    nacK = instantiateNacMorphisms (domain (left l)) (domain (left nacRule)) mapL
    nacR = instantiateNacMorphisms (codomain (right l)) (codomain (right nacRule)) mapR

instantiateNacMorphisms :: TypedGraph a b -> TypedGraph a b
                        -> [Mapping] -> TypedGraphMorphism a b
instantiateNacMorphisms graphL graphN mapping = typedMorphism graphL graphN maps
  where
    mapElements = map (\(x,_,y) -> (read y :: Int, read x :: Int)) mapping
    maps = gmbuild
             (domain graphL)
             (domain graphN)
             mapElements
             mapElements

instantiateRuleMorphisms :: (SndOrderRuleSide, GraphRule a b)
                         -> (SndOrderRuleSide, GraphRule a b)
                         -> (RuleMorphism a b , RuleMorphism a b)
instantiateRuleMorphisms (parsedLeft, l) (parsedRight, r) =
  (ruleMorphism ruleK l leftKtoLeftL interfaceKtoL rightKtoRightL,
   ruleMorphism ruleK r leftKtoLeftR interfaceKtoR rightKtoRightR)
    where
      graphKRuleL = domain (left l)
      graphKRuleR = domain (left r)
      graphLRuleL = codomain (left l)
      graphLRuleR = codomain (left r)
      graphRRuleL = codomain (right l)
      graphRRuleR = codomain (right r)
      
      mappingBetweenLeft = SO.getLeftObjNameMapping parsedLeft parsedRight
      mappingBetweenRight = SO.getRightObjNameMapping parsedLeft parsedRight
      
      ruleK = production leftK rightK []
      
      graphLRuleK = domain leftKtoLeftL
      graphRRuleK = domain rightKtoRightL
      
      (leftKtoLeftL, leftKtoLeftR) =
        instantiateSpan graphLRuleL graphLRuleR mappingBetweenLeft
      
      (interfaceKtoL, interfaceKtoR) =
        instantiateSpan graphKRuleL graphKRuleR mappingBetweenLeft
      
      (rightKtoRightL, rightKtoRightR) =
        instantiateSpan graphRRuleL graphRRuleR mappingBetweenRight
      
      maps (_,_,(_,_,_,x)) = x
      (leftK, rightK) = instantiateSpan graphLRuleK graphRRuleK (maps parsedLeft)
