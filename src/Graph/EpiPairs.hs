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

  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes, considering they have same domain
  commutingPairs inj m1 m2 = filt
    where
      allPairs = createPairs inj (codomain m1) (codomain m2)
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs
      
  -- | Create all jointly surjective pairs of @m1@ and @m2@ that commutes,
  -- considering they have same domain
  -- and flags indicating the injective of each morphism
  commutingPairsAlt (m1,inj1) (m2,inj2) = filt
    where
      cod1 = codomain m1
      cod2 = codomain m2
      allPairs = map (mountTGMBoth cod1 cod2) (genGraphEqClass (mixGM (cod1,inj1) (cod2,inj2)))
      filt = filter (\(x,y) -> compose m1 x == compose m2 y) allPairs

doL :: Bool -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> GraphMorphism a b
            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> GraphMorphism a b
            -> [(TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b)]
doL inj k1 leftM1 l1 k2 leftM2 l2 = lsss
  where
    ls = createPairs inj l1 l2
    lss = concatMap (\(l1,l2) -> sequence [[l1],[l2], matches MONO (codomain k1) (codomain l1)]) ls
    lss2 = map (\(x:y:z:_) -> (x,y,z)) lss
    lsss = filter (\(ll1,ll2,m) -> compose leftM1 ll1 == compose k1 m &&
                                   compose leftM2 ll2 == compose k2 m) lss2

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
                (\(k1,k2) -> let ls = doL inj k1 (left m1) l1 k2 (left m2) l2
                             in map (\(ll1,ll2,m) -> (k1, k2, ll1, ll2, m)) ls) ks
      rights = concatMap
                (\(k1,k2,ll1,ll2,l) -> let rs = doL inj k1 (right m1) r1 k2 (right m2) r2
                                       in map (\(rr1,rr2,r) -> (k1,k2,ll1,ll2,l,rr1,rr2,r)) rs) lefts
      
      ret = map (\(k1,k2,l1,l2,l,r1,r2,r) ->
                   let rule = production l r []
                   in (ruleMorphism m1 rule l1 k1 r1,
                       ruleMorphism m2 rule l2 k2 r2)) rights
  
  -- FIXME
  createPairsNac _ inj r nac = createPairs inj r (codomain nac)
  
  -- FIXME
  commutingPairs inj m1 m2 = createPairs inj (codomain m1) (codomain m2)
  
  -- FIXME
  commutingPairsAlt (m1,inj1) (m2,_) = createPairs inj1 (codomain m1) (codomain m2)
