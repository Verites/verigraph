module TypedGraph.GraphProcess (
singleProcess
                               )

where

import Abstract.Cocomplete
import Abstract.DPO
import Abstract.DPO.Process
import Abstract.DPO.Derivation
import Abstract.Morphism
import TypedGraph.Morphism

singleProcess :: Derivation (TypedGraphMorphism a b) -> Process (TypedGraphMorphism a b)
singleProcess derivation =
  let d1 = dToG derivation
      d2 = dToH derivation
      (d1',d2') = calculatePushout d1 d2
      core = codomain d1'
      rule = production derivation
      oldL = getLHS rule
      oldR = getRHS rule
      mappingL = mapping oldL
      mappingR = mapping oldR
      m = match derivation
      h = comatch derivation
      newLType = compose (mapping m) (mapping d2')
      newRType = compose (mapping h) (mapping d1')
      newKType = compose mappingL newLType
      newL = buildTypedGraphMorphism newKType newLType mappingL
      newR = buildTypedGraphMorphism newKType newRType mappingR
      newProduction = buildProduction newL newR []
   in Process [newProduction] core

