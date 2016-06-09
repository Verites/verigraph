{-# LANGUAGE TypeFamilies #-}

module Graph.RuleMorphism (
    RuleMorphism
  , ruleMorphism
  , mappingLeft
  , mappingInterface
  , mappingRight
  , commutingMorphismSameDomain
  , commutingMorphismSameCodomain
  ) where

import Abstract.AdhesiveHLR
import Abstract.DPO
import Abstract.Morphism
import Abstract.Valid
import Graph.GraphRule ()
import Graph.TypedGraphMorphism

-- | A morphism between two rules:
-- (desconsidering the NACs)
--
-- @
--           l1      r1
--       L1◀─────K1─────▶R1
--       │       │       │
--  mapL │   mapK│   mapR│
--       ▼       ▼       ▼
--       L2◀─────K2─────▶R2
--           l2      r2
-- @
--
-- getDomain = (l1,r1)
--
-- getCodomain = (l2,r2)
--
-- mappingLeft = mapL
--
-- mappingInterface = mapK
--
-- mappingRight = mapR
--
data RuleMorphism a b =
  RuleMorphism {
    getDomain        :: Production (TypedGraphMorphism a b)
  , getCodomain      :: Production (TypedGraphMorphism a b)
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Show)

ruleMorphism :: Production (TypedGraphMorphism a b)
             -> Production (TypedGraphMorphism a b)
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> RuleMorphism a b
ruleMorphism = RuleMorphism

-- aux functions of matches
leftM :: FindMorphism t => PROP -> Production t -> Production t -> t -> [(t, t)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = matches prop (codomain (left l)) (codomain (left g))
    commuting = filter (\m -> compose (left l) m == compose mapK (left g)) matchesL

rightM :: FindMorphism t =>  PROP -> Production t -> Production t -> (t, t) -> [(t, t, t)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = matches prop (codomain (right l)) (codomain (right g))
    commuting = filter (\m -> compose (right l) m == compose mapK (right g)) matchesR

buildPair :: Production (TypedGraphMorphism a b)
        -> Production (TypedGraphMorphism a b)
        -> (TypedGraphMorphism a b,
            TypedGraphMorphism a b,
            TypedGraphMorphism a b)
        -> RuleMorphism a b
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3

instance FindMorphism (RuleMorphism a b) where
  -- | A match between two rules, only considers monomorphic matches morphisms:
  -- (desconsidering the NACs)
  matches prop l g = map (buildPair l g) rightMatch
    where
      matchesK = matches prop (domain (left l)) (domain (left g))
      leftMatch = concatMap (leftM prop l g) matchesK
      rightMatch = concatMap (rightM prop l g) leftMatch
  
  partInjMatches n m =
    filter
      (\q ->
        (partiallyMonomorphic (mappingLeft n) (mappingLeft q)) &&
        (partiallyMonomorphic (mappingInterface n) (mappingInterface q)) &&
        (partiallyMonomorphic (mappingRight n) (mappingRight q)))
      (matches ALL (codomain n) (codomain m))

{- brute-force
getAllMaps :: PROP -> Production (TypedGraphMorphism a b)
           -> Production (TypedGraphMorphism a b) -> [RuleMorphism a b]
getAllMaps prop l g =
  do
    let f (mapL,mapk,mapR) = RuleMorphism l g mapL mapk mapR
    matchesL <- matches prop (codomain (left l)) (codomain (left g))
    matchesK <- matches prop (domain (left l)) (domain (left g))
    matchesR <- matches prop (codomain (right l)) (codomain (right g))
    return $ f (matchesL, matchesK, matchesR)-}

-- | Given the morphisms /k1 : X -> Y/, /s1 : X -> Z/,
-- /k2 : W -> Y/ and /s2 : W -> Z/, respectively,
-- creates the monomorphic morphism /x : Y -> Z/,
-- where the following diagram commutes:
--
-- @
--        k1
--     X ───▶Y
--      \\   / ▲
--     s1\\ /x  \\k2
--        ▼     \\
--        Z◀──── W
--           s2
-- @
-- 
commutingMorphismSameDomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameDomain k1 s1 k2 s2 = typedMorphism (codomain k1) (codomain s1) select
  where
    mats = matches MONO (codomain k1) (codomain s1)
    filt = filter (\m -> compose k1 m == s1 && compose k2 m == s2) mats
    select = if Prelude.null filt
               then error "(domain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
               else if length filt > 1
                   then error "(domain) Error when commuting monomorphic morphisms (non unique commuting morphism)"
                   else mapping (head filt)

-- | Given the morphisms /k1 : Y -> X/, /s1 : Z -> X/,
-- /k2 : W -> Y/ and /s2 : W -> Z/, respectively,
-- creates the monomorphic morphism /a : X -> Y/,
-- where the following diagram commutes:
--
-- @
--        k1
--     X ◀─── Y
--     ▲     / ▲
--    s1\\   /x  \\k2
--       \\ ▼     \\
--        Z◀──── W
--           s2
-- @
-- 
commutingMorphismSameCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                              -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameCodomain k1 s1 k2 s2 = typedMorphism (domain k1) (domain s1) select
  where
    mats = matches MONO (domain k1) (domain s1)
    filt = filter (\m -> compose m s1 == k1 && compose k2 m == s2) mats
    select = if Prelude.null filt
               then error "(codomain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
               else if length filt > 1
                   then error "(codomain) Error when commuting monomorphic morphisms (non unique commuting morphism)"
                   else mapping (head filt)

instance AdhesiveHLR (RuleMorphism a b) where
  -- poc m l
  poc (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK _ leftL leftK leftR) = (k,l')
     where
       (matchL', leftL') = poc matchL leftL
       (matchK', leftK') = poc matchK leftK
       (matchR', leftR') = poc matchR leftR
       l = commutingMorphismSameCodomain
             (compose leftK' (left ruleG)) leftL'
             matchK' (compose (left ruleK) matchL')
       r = commutingMorphismSameCodomain
             (compose leftK' (right ruleG)) leftR'
             matchK' (compose (right ruleK) matchR')
       newRule = production l r []
       k = RuleMorphism ruleK newRule matchL' matchK' matchR'
       l' = RuleMorphism newRule ruleG leftL' leftK' leftR'
  
  -- po k r
  po (RuleMorphism _ ruleD matchL matchK matchR) (RuleMorphism _ ruleR rightL rightK rightR) =  (m',r')
     where
       (matchL', rightL') = po matchL rightL
       (matchK', rightK') = po matchK rightK
       (matchR', rightR') = po matchR rightR
       l = commutingMorphismSameDomain
             rightK' (compose (left ruleD) rightL')
             matchK' (compose (left ruleR) matchL')
       r = commutingMorphismSameDomain
             rightK' (compose (right ruleD) rightR')
             matchK' (compose (right ruleR) matchR')
       newRule = production l r []
       m' = RuleMorphism ruleR newRule matchL' matchK' matchR'
       r' = RuleMorphism ruleD newRule rightL' rightK' rightR'
  
  -- TODO
  injectivePullback _ _ = error "injectivePullback not implemented in RuleMorphism"

instance Eq (RuleMorphism a b) where
    (RuleMorphism dom1 cod1 mapL1 mapK1 mapR1) == (RuleMorphism dom2 cod2 mapL2 mapK2 mapR2) =
        dom1 == dom2 &&
        cod1 == cod2 &&
        mapL1 == mapL2 &&
        mapK1 == mapK2 &&
        mapR1 == mapR2

instance Morphism (RuleMorphism a b) where
    type Obj (RuleMorphism a b) = Production (TypedGraphMorphism a b)

    domain = getDomain
    codomain = getCodomain
    
    compose t1 t2 =
        RuleMorphism (domain t1)
                     (codomain t2)
                     (compose (mappingLeft t1) (mappingLeft t2))
                     (compose (mappingInterface t1) (mappingInterface t2))
                     (compose (mappingRight t1) (mappingRight t2))
    
    id t = RuleMorphism t t
             (idMap (codomain (left t)) (codomain (left t)))
             (idMap (domain (left t)) (domain (left t)))
             (idMap (codomain (right t)) (codomain (right t)))

    monomorphism rm =
      monomorphism (mappingLeft rm) &&
      monomorphism (mappingInterface rm) &&
      monomorphism (mappingRight rm)
    
    epimorphism rm =
      epimorphism (mappingLeft rm) &&
      epimorphism (mappingInterface rm) &&
      epimorphism (mappingRight rm)
    
    isomorphism (RuleMorphism dom cod mapL mapK mapR) =
      isomorphism mapL &&
      isomorphism mapK &&
      isomorphism mapR &&
      compose (left dom) mapL == compose mapK (left cod) &&
      compose (right dom) mapR == compose mapK (right cod)

instance Valid (RuleMorphism a b) where
    valid (RuleMorphism dom cod mapL mapK mapR) =
        valid dom &&
        valid cod &&
        valid mapL &&
        valid mapK &&
        valid mapR &&
        compose mapK (left cod) == compose (left dom) mapL &&
        compose mapK (right cod) == compose (right dom) mapR
