{-# LANGUAGE TypeFamilies #-}

module Graph.RuleMorphism (
    RuleMorphism
  , ruleMorphism
  , mappingLeft
  , mappingInterface
  , mappingRight
  , matchesSndOrder
  , commutingMorphismSameDomain
  ) where

import Abstract.AdhesiveHLR
import Abstract.DPO
import Abstract.Morphism
import Abstract.Valid
import Graph.GraphRule
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
    getDomain        :: GraphRule a b
  , getCodomain      :: GraphRule a b
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Show)


ruleMorphism :: GraphRule a b -> GraphRule a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> RuleMorphism a b
ruleMorphism = RuleMorphism

instance FindMorphism (RuleMorphism a b) where
  matches = matchesSndOrder
  partInjMatches = error "Partial Injective Matches for RuleMorphism not implemented"

-- | A match between two rules, only considers monomorphic matches morphisms:
--
-- (desconsidering the NACs)
matchesSndOrder :: PROP -> GraphRule a b -> GraphRule a b -> [RuleMorphism a b]
matchesSndOrder prop l g = {-error (show ((getAllMaps prop l g)!!3))-} filter valid (getAllMaps prop l g)

getAllMaps :: PROP -> GraphRule a b -> GraphRule a b -> [RuleMorphism a b]
getAllMaps prop l g =
  do
    let f (mapL,mapk,mapR) = RuleMorphism l g mapL mapk mapR
    matchesL <- matches prop (codomain (left l)) (codomain (left g))
    matchesK <- matches prop (domain (left l)) (domain (left g))
    matchesR <- matches prop (codomain (right l)) (codomain (right g))
    return $ f (matchesL, matchesK, matchesR)

instance DPO (RuleMorphism a b) where
  satsGluing inj m prod =
    satsGluing inj (mappingLeft m)      leftProd      &&
    satsGluing inj (mappingInterface m) interfaceProd &&
    satsGluing inj (mappingRight m)     rightProd     &&
    True -- dangling span to do
      where
        leftProd = production (mappingLeft (left prod)) (mappingLeft (right prod)) []
        interfaceProd = production (mappingInterface (left prod)) (mappingInterface (right prod)) []
        rightProd = production (mappingRight (left prod)) (mappingRight (right prod)) []

  partiallyMonomorphic = error "partiallyMonomorphic not implemented for RuleMorphism"

-- | Given the morphisms /a : X -> Y/ and /b : X -> Z/, respectively,
-- creates the monomorphic morphism /x : Y -> Z/,
-- where the following diagram commutes:
--
-- @
--        a
--     X ───▶Y
--      \\   /
--      b\\ /x
--        ▼ 
--        Z
-- @
-- 
commutingMorphismSameDomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameDomain a b = typedMorphism (codomain a) (codomain b) select
  where
    mats = matches MONO (codomain a) (codomain b)
    filt = filter (\m -> compose a m == b) mats
    select = if Prelude.null filt
               then error "Error when commuting morphisms with same domain"
               else mapping (head filt)

instance AdhesiveHLR (RuleMorphism a b) where
  -- poc m l
  poc (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK _ leftL leftK leftR) = (k,l')
     where
       (matchL', leftL') = poc matchL leftL
       (matchK', leftK') = poc matchK leftK
       (matchR', leftR') = poc matchR leftR
       l = commutingMorphismSameDomain leftK' (compose (left ruleG) leftL')
       r = commutingMorphismSameDomain leftK' (compose (right ruleG) leftR')
       newRule = graphRule l r []
       k = RuleMorphism ruleK newRule matchL' matchK' matchR'
       l' = RuleMorphism newRule ruleG leftL' leftK' leftR'
  
  -- po k r
  po (RuleMorphism _ ruleD matchL matchK matchR) (RuleMorphism _ ruleR rightL rightK rightR) =  (m',r')
     where
       (matchL', rightL') = po matchL rightL
       (matchK', rightK') = po matchK rightK
       (matchR', rightR') = po matchR rightR
       l = commutingMorphismSameDomain rightK' (compose (left ruleD) rightL')
       r = commutingMorphismSameDomain rightK' (compose (right ruleD) rightR')
       newRule = graphRule l r []
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
    type Obj (RuleMorphism a b) = GraphRule a b

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
    
    isomorphism rm = -- check if needs to commute
      isomorphism (mappingLeft rm) &&
      isomorphism (mappingInterface rm) &&
      isomorphism (mappingRight rm)

instance Valid (RuleMorphism a b) where
    valid (RuleMorphism dom cod mapL mapK mapR) =
        valid dom &&
        valid cod &&
        valid mapL &&
        valid mapK &&
        valid mapR &&
        compose mapK (left cod) == compose (left dom) mapL &&
        compose mapK (right cod) == compose (right dom) mapR
