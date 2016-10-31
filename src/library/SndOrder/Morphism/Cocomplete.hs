{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SndOrder.Morphism.Cocomplete where

import           Abstract.Cocomplete
import           Abstract.DPO
import           Abstract.Morphism as M

import           TypedGraph.Graph
import           TypedGraph.Morphism

import           SndOrder.Morphism.Core
import           SndOrder.Morphism.AdhesiveHLR

instance Cocomplete (RuleMorphism a b) where
  
  calculateCoequalizer = error "asd"
  
  calculateNCoequalizer = error "asd"
  
  calculateCoproduct rule1 rule2 = (m1,m2)
    where
      (l1,l2) = coproductCod (getLHS rule1) (getLHS rule2)
      (k1,k2) = coproductDom (getLHS rule1) (getLHS rule2)
      (r1,r2) = coproductCod (getRHS rule1) (getRHS rule2)
      
      l = commutingMorphism k1 (compose (getLHS rule1) l1) k2 (compose (getLHS rule2) l2)
      r = commutingMorphism k1 (compose (getRHS rule1) r1) k2 (compose (getRHS rule2) r2) 
      
      coproductRule = buildProduction l r []
      
      m1 = RuleMorphism rule1 coproductRule l1 k1 r1
      m2 = RuleMorphism rule2 coproductRule l2 k2 r2
  
  calculateNCoproduct = error "asd"
  
  initialObject morph = buildProduction (M.id initGraph) (M.id initGraph) []
    where
      initGraph = initialObject (getLHS (domain morph))

coproductCod :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
coproductCod a b = calculateCoproduct (codomain a) (codomain b)

coproductDom :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
coproductDom a b = calculateCoproduct (domain a) (domain b)
