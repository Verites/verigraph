module TypedGraph.DPO.GraphProcess

where

import Abstract.DPO
import Abstract.Morphism
import TypedGraph.DPO.GraphRule ()
import TypedGraph.Graph ()
import TypedGraph.Morphism as TGM

instance GenerateProcess (TypedGraphMorphism a b) where
  retype = retype'

retype' :: (Derivation (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  Production (TypedGraphMorphism a b)
retype' (derivation, (g1,_,g3)) = newProduction
  where
    p = production derivation
    oldL = getLHS p
    oldR = getRHS p
    mappingL = mapping oldL
    mappingR = mapping oldR
    m = match derivation
    h = comatch derivation
    newLType = compose (mapping m) (mapping g1)
    newRType = compose (mapping h) (mapping g3)
    newKType = compose mappingL newLType -- change it to use cl2
    newL = buildTypedGraphMorphism newKType newLType mappingL
    newR = buildTypedGraphMorphism newKType newRType mappingR
    newProduction = buildProduction newL newR []
