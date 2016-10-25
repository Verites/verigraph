{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module SndOrder.Morphism.EpiPairs where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Abstract.Valid
import           Graph.GraphMorphism  hiding (applyEdge, applyEdgeUnsafe,
                                       applyNode, applyNodeUnsafe)
import           TypedGraph.GraphRule
import           TypedGraph.Morphism

import           SndOrder.Morphism.Core

instance EpiPairs (RuleMorphism a b) where
  createJointlyEpimorphicPairs inj m1 m2 = ret
    where
      l1 = codomain (getLHS m1)
      l2 = codomain (getLHS m2)
      k1 = domain (getLHS m1)
      k2 = domain (getLHS m2)
      r1 = codomain (getRHS m1)
      r2 = codomain (getRHS m2)

      ks = createJointlyEpimorphicPairs inj k1 k2

      lefts = concatMap
                (\(k1,k2) -> let ls = createSideRule inj k1 (getLHS m1) l1 k2 (getLHS m2) l2
                             in map (\(ll1,ll2,m) -> (k1, k2, ll1, ll2, m)) ls) ks
      rights = concatMap
                (\(k1,k2,ll1,ll2,l) -> let rs = createSideRule inj k1 (getRHS m1) r1 k2 (getRHS m2) r2
                                       in map (\(rr1,rr2,r) -> (k1,k2,ll1,ll2,l,rr1,rr2,r)) rs) lefts

      ret = map (\(k1,k2,l1,l2,l,r1,r2,r) ->
                   let rule = buildProduction l r []
                   in (ruleMorphism m1 rule l1 k1 r1,
                       ruleMorphism m2 rule l2 k2 r2)) rights

  createAllSubobjects _ _ = error "CreateAllSubobjects for RuleMorphism: Not implemented"

  --FIXME
  createJointlyEpimorphicPairsFromNAC _ r nac = allPairs
    where
      allPairs = createJointlyEpimorphicPairs True r (codomain nac)

  calculateCommutativeSquaresAlongMonomorphism (m1,inj1) (m2,inj2) = filt
    where
      allCommutingPairs = calculateCommutativeSquares False m1 m2
      satsM1 = if inj1 then isMonomorphism else const True
      satsM2 = if inj2 then isMonomorphism else const True
      filt = filter (\(m1,m2) -> satsM1 m1 && satsM2 m2) allCommutingPairs

-- | Generates all (ss1,ss2,m) morphisms that commute with all EpiPairs of S1 and S2.
-- The Bool flag indicates monomorphics ss1 and ss2.
-- The morphism m is always monomorphic.
--
-- @
--        sideM1
--      K1─────▶S1
--      │       :
--    k1│       :ss1
--      ▼   m   ▼
--      K------▶S
--      ▲       ▲
--    k2│       :ss2
--      │       :
--      K2─────▶S2
--        sideM2
-- @
createSideRule :: Bool -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> GraphMorphism a b
            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> GraphMorphism a b
            -> [(TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b)]
createSideRule inj k1 sideM1 s1 k2 sideM2 s2 = d
  where
    a = createJointlyEpimorphicPairs inj s1 s2
    b = concatMap (\(s1,s2) -> sequence [[s1],[s2], findMonomorphisms (codomain k1) (codomain s1)]) a
    c = map (\(x:y:z:_) -> (x,y,z)) b
    d = filter (\(ss1,ss2,m) -> compose sideM1 ss1 == compose k1 m &&
                                compose sideM2 ss2 == compose k2 m) c

