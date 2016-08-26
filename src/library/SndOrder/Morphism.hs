{-# LANGUAGE TypeFamilies #-}

module SndOrder.Morphism (
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
import Graph.GraphMorphism
import TypedGraph.Morphism
import TypedGraph.GraphRule

-- | A morphism between two rules.
--
-- The following diagram illustrates such a morphism, omiting the NACs.
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
-- domain = (l1,r1)
--
-- codomain = (l2,r2)
--
-- mappingLeft = mapL
--
-- mappingInterface = mapK
--
-- mappingRight = mapR
--
-- TODO: Make polymorphic on the type of morphism?
data RuleMorphism a b =
  RuleMorphism {
    rmDomain   :: Production (TypedGraphMorphism a b)
  , rmCodomain :: Production (TypedGraphMorphism a b)
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Eq, Show, Read)

ruleMorphism :: Production (TypedGraphMorphism a b)
             -> Production (TypedGraphMorphism a b)
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> RuleMorphism a b
ruleMorphism = RuleMorphism

instance Valid (RuleMorphism a b) where
    valid (RuleMorphism dom cod mapL mapK mapR) =
        valid dom &&
        valid cod &&
        valid mapL &&
        valid mapK &&
        valid mapR &&
        compose mapK (left cod) == compose (left dom) mapL &&
        compose mapK (right cod) == compose (right dom) mapR

instance Morphism (RuleMorphism a b) where
    type Obj (RuleMorphism a b) = Production (TypedGraphMorphism a b)

    domain = rmDomain
    codomain = rmCodomain

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


instance FindMorphism (RuleMorphism a b) where
  -- | A match between two rules, only considers monomorphic matches morphisms:
  -- (desconsidering the NACs)
  findMorphisms prop l g = map (buildPair l g) rightMatch
    where
      matchesK = findMorphisms prop (domain (left l)) (domain (left g))
      leftMatch = concatMap (leftM prop l g) matchesK
      rightMatch = concatMap (rightM prop l g) leftMatch

  partInjMatches n m =
    filter
      (\q ->
        partiallyMonomorphic (mappingLeft n) (mappingLeft q) &&
        partiallyMonomorphic (mappingInterface n) (mappingInterface q) &&
        partiallyMonomorphic (mappingRight n) (mappingRight q))
      (findMorphisms AnyMorphisms (codomain n) (codomain m))

-- commutes left side
leftM :: FindMorphism t => MorphismRestriction -> Production t -> Production t -> t -> [(t, t)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = findMorphisms prop (codomain (left l)) (codomain (left g))
    commuting = filter (\m -> compose (left l) m == compose mapK (left g)) matchesL

-- commutes right side
rightM :: FindMorphism t =>  MorphismRestriction -> Production t -> Production t -> (t, t) -> [(t, t, t)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = findMorphisms prop (codomain (right l)) (codomain (right g))
    commuting = filter (\m -> compose (right l) m == compose mapK (right g)) matchesR

-- kind of curry for three arguments
buildPair :: Production (TypedGraphMorphism a b)
        -> Production (TypedGraphMorphism a b)
        -> (TypedGraphMorphism a b,
            TypedGraphMorphism a b,
            TypedGraphMorphism a b)
        -> RuleMorphism a b
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3


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
                   let rule = constructProduction l r []
                   in (ruleMorphism m1 rule l1 k1 r1,
                       ruleMorphism m2 rule l2 k2 r2)) rights

  partitions _ _ = error "Not implemented"

  --FIXME
  createPairsNac _ r nac = allPairs
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
      satsM1 = if inj1 then monomorphism else const True
      satsM2 = if inj2 then monomorphism else const True
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
    a = createPairs inj s1 s2
    b = concatMap (\(s1,s2) -> sequence [[s1],[s2], findMorphisms MonoMorphisms (codomain k1) (codomain s1)]) a
    c = map (\(x:y:z:_) -> (x,y,z)) b
    d = filter (\(ss1,ss2,m) -> compose sideM1 ss1 == compose k1 m &&
                                compose sideM2 ss2 == compose k2 m) c


instance AdhesiveHLR (RuleMorphism a b) where
  pushoutComplement (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK _ leftL leftK leftR) = (k,l')
     where
       (matchL', leftL') = pushoutComplement matchL leftL
       (matchK', leftK') = pushoutComplement matchK leftK
       (matchR', leftR') = pushoutComplement matchR leftR
       l = commutingMorphismSameCodomain
             (compose leftK' (left ruleG)) leftL'
             matchK' (compose (left ruleK) matchL')
       r = commutingMorphismSameCodomain
             (compose leftK' (right ruleG)) leftR'
             matchK' (compose (right ruleK) matchR')
       newRule = constructProduction l r []
       k = RuleMorphism ruleK newRule matchL' matchK' matchR'
       l' = RuleMorphism newRule ruleG leftL' leftK' leftR'

  pushout (RuleMorphism _ ruleD matchL matchK matchR) (RuleMorphism _ ruleR rightL rightK rightR) =  (m',r')
     where
       (matchL', rightL') = pushout matchL rightL
       (matchK', rightK') = pushout matchK rightK
       (matchR', rightR') = pushout matchR rightR
       l = commutingMorphismSameDomain
             rightK' (compose (left ruleD) rightL')
             matchK' (compose (left ruleR) matchL')
       r = commutingMorphismSameDomain
             rightK' (compose (right ruleD) rightR')
             matchK' (compose (right ruleR) matchR')
       newRule = constructProduction l r []
       m' = RuleMorphism ruleR newRule matchL' matchK' matchR'
       r' = RuleMorphism ruleD newRule rightL' rightK' rightR'

  -- TODO
  injectivePullback _ _ = error "injectivePullback not implemented in RuleMorphism"

  hasPushoutComplement (restrictionG, g) (restrictionF, f) =
    hasPushoutComplement (restrictionG, mappingLeft g) (restrictionF, mappingLeft f)
    && hasPushoutComplement (restrictionG, mappingRight g) (restrictionF, mappingRight f)
    && hasPushoutComplement (restrictionG, mappingInterface g) (restrictionF, mappingInterface f)
    && danglingSpan (left $ codomain g) (mappingLeft g) (mappingInterface g) (mappingLeft f) (mappingInterface f)
    && danglingSpan (right $ codomain g) (mappingRight g) (mappingInterface g) (mappingRight f) (mappingInterface f)

-- | A gluing condition for pushout complements of rule morphisms
danglingSpan :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
danglingSpan matchRuleSide matchMorp matchK l k = deletedNodesInK && deletedEdgesInK
  where
    deletedNodes = filter (ruleDeletes l matchMorp applyNodeTGM nodesDomain) (nodesCodomain matchMorp)
    nodesInK = [a | a <- nodesDomain matchRuleSide, applyNodeTGMUnsafe matchRuleSide a `elem` deletedNodes]
    deletedNodesInK = all (ruleDeletes k matchK applyNodeTGM nodesDomain) nodesInK

    deletedEdges = filter (ruleDeletes l matchMorp applyEdgeTGM edgesDomain) (edgesCodomain matchMorp)
    edgesInK = [a | a <- edgesDomain matchRuleSide, applyEdgeTGMUnsafe matchRuleSide a `elem` deletedEdges]
    deletedEdgesInK = all (ruleDeletes k matchK applyEdgeTGM edgesDomain) edgesInK


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
-- TODO: explain the errors in this function. what are preconditions for them not to occur?!?
commutingMorphismSameDomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameDomain k1 s1 k2 s2 = typedMorphism (codomain k1) (codomain s1) select
  where
    mats = findMorphisms MonoMorphisms (codomain k1) (codomain s1)
    filt = filter (\m -> compose k1 m == s1 && compose k2 m == s2) mats
    select = case filt of
                [] -> error "(domain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(domain) Error when commuting monomorphic morphisms (non unique commuting morphism)"

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
-- TODO: explain the errors in this function. what are preconditions for them not to occur?!?
commutingMorphismSameCodomain :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                              -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphismSameCodomain k1 s1 k2 s2 = typedMorphism (domain k1) (domain s1) select
  where
    mats = findMorphisms MonoMorphisms (domain k1) (domain s1)
    filt = filter (\m -> compose m s1 == k1 && compose k2 m == s2) mats
    select = case filt of
                [] -> error "(domain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(domain) Error when commuting monomorphic morphisms (non unique commuting morphism)"
