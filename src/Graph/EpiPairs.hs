{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graph.EpiPairs where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.GraphMorphism      (GraphMorphism)
import           Graph.TypedGraphMorphism (TypedGraphMorphism)
import           Graph.RuleMorphism       (RuleMorphism, ruleMorphism)
import           Partitions.GPToVeri      (mountTGMBoth)
import           Partitions.GraphPart     (genGraphEqClass)
import           Partitions.VeriToGP      (mixGM,mixNac)

instance EpiPairs (TypedGraphMorphism a b) where
  -- | Create all jointly surjective pairs of @m1@ and @m2@
  createPairs inj m1 m2 = map (mountTGMBoth m1 m2) (genGraphEqClass (mixGM (m1,inj) (m2,inj)))

  -- | Create all jointly surjective pairs of @m1@ and @m2@ with some of both injective
  --createPairsAlt (m1,inj1) (m2,inj2) = map (mountTGMBoth m1 m2) (genGraphEqClass (mixGM (m1,inj1) (m2,inj2)))
  
  createPairsNac nacInj inj r nac = map (mountTGMBoth r (codomain nac)) (genGraphEqClass (mixNac (r,inj) (nac,nacInj)))
      
  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes,
  -- considering they have same domain
  -- and flags indicating the injective of each morphism
  commutingPairsAlt (m1,inj1) (m2,inj2) = filt
    where
      cod1 = codomain m1
      cod2 = codomain m2
      allPairs = map (mountTGMBoth cod1 cod2) (genGraphEqClass (mixGM (cod1,inj1) (cod2,inj2)))
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs

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
    a = createPairs inj s1 s2
    b = concatMap (\(s1,s2) -> sequence [[s1],[s2], matches MONO (codomain k1) (codomain s1)]) a
    c = map (\(x:y:z:_) -> (x,y,z)) b
    d = filter (\(ss1,ss2,m) -> compose sideM1 ss1 == compose k1 m &&
                                compose sideM2 ss2 == compose k2 m) c

instance EpiPairs (RuleMorphism a b) where
  createPairs inj m1 m2 = ret
    where
      l1 = codomain (left m1)
      l2 = codomain (left m2)
      k1 = domain (left m1)
      k2 = domain (left m2)
      r1 = codomain (right m1)
      r2 = codomain (right m2)
      
      ks = createPairs inj k1 k2
      
      lefts = concatMap
                (\(k1,k2) -> let ls = createSideRule inj k1 (left m1) l1 k2 (left m2) l2
                             in map (\(ll1,ll2,m) -> (k1, k2, ll1, ll2, m)) ls) ks
      rights = concatMap
                (\(k1,k2,ll1,ll2,l) -> let rs = createSideRule inj k1 (right m1) r1 k2 (right m2) r2
                                       in map (\(rr1,rr2,r) -> (k1,k2,ll1,ll2,l,rr1,rr2,r)) rs) lefts
      
      ret = map (\(k1,k2,l1,l2,l,r1,r2,r) ->
                   let rule = production l r []
                   in (ruleMorphism m1 rule l1 k1 r1,
                       ruleMorphism m2 rule l2 k2 r2)) rights
  
  --FIXME
  createPairsNac _ _ r nac = allPairs
    where
      allPairs = createPairs True r (codomain nac)
  {-createPairsNac nacInj inj r nac = satsMorphisms
    where
      allPairs = createPairs False r (codomain nac)
      satsR = if inj then monomorphism else (\_ -> True)
      satsNac = if nacInj then monomorphism else partiallyMonomorphic nac
      satsMorphisms = filter (\(h,q) -> satsR h && satsNac q) allPairs-}
  
  commutingPairsAlt (m1,inj1) (m2,inj2) = filt
    where
      allCommutingPairs = commutingPairs False m1 m2
      satsM1 = if inj1 then monomorphism else (\_ -> True)
      satsM2 = if inj2 then monomorphism else (\_ -> True)
      filt = filter (\(m1,m2) -> satsM1 m1 && satsM2 m2) allCommutingPairs
