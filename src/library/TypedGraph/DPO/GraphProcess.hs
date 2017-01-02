module TypedGraph.DPO.GraphProcess

where

import Abstract.DPO
import Abstract.Morphism
import Data.List
import TypedGraph.DPO.GraphRule ()
import TypedGraph.Graph ()
import TypedGraph.Morphism as TGM

instance GenerateProcess (TypedGraphMorphism a b) where
  typing = retypeProduction
  productionTyping = retype
  restrictMorphisms = restrictMorphisms'

retypeProduction :: (Derivation (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  Production (TypedGraphMorphism a b)
retypeProduction (derivation, (g1,_,g3)) = newProduction
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
    newKType = compose mappingL newLType -- change it to use gluing and g2?
    newL = buildTypedGraphMorphism newKType newLType mappingL
    newR = buildTypedGraphMorphism newKType newRType mappingR
    newProduction = buildProduction newL newR []

retype :: (Production (TypedGraphMorphism a b), (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b)) ->  Production (TypedGraphMorphism a b)
retype (p, (g1,g2,g3)) = newProduction
  where
    oldL = getLHS p
    oldR = getRHS p
    newKType = mapping g2
    newL = buildTypedGraphMorphism newKType (mapping g1) (mapping oldL)
    newR = buildTypedGraphMorphism newKType (mapping g3) (mapping oldR)
    newProduction = buildProduction newL newR []

restrictMorphisms' :: (TypedGraphMorphism a b, TypedGraphMorphism a b) -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
restrictMorphisms' (a,b) = (removeOrphans a, removeOrphans b)
  where
    orphanNodes = orphanTypedNodes a `intersect` orphanTypedNodes b
    orphanEdges = orphanTypedEdges a `intersect` orphanTypedEdges b
    removeOrphans m = foldr removeNodeFromCodomain (foldr removeEdgeFromCodomain m orphanEdges) orphanNodes
