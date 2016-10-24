{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module SndOrder.Morphism (
    RuleMorphism
  , ruleMorphism
  , mappingLeft
  , mappingInterface
  , mappingRight
  , commutingMorphismSameDomain
  , commutingMorphismSameCodomain
  ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           Abstract.Valid
import           Graph.GraphMorphism  hiding (applyEdge, applyEdgeUnsafe,
                                       applyNode, applyNodeUnsafe)
import           TypedGraph.GraphRule
import           TypedGraph.Morphism

-- | A morphism between two first order rules.
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
    rmDomain         :: Production (TypedGraphMorphism a b)
  , rmCodomain       :: Production (TypedGraphMorphism a b)
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
    validate (RuleMorphism dom cod mapL mapK mapR) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , withContext "left-hand graph morphism" (validate mapL)
        , withContext "interface graph morphism" (validate mapK)
        , withContext "right-hand graph morphism" (validate mapR)
        , ensure (compose mapK (getLHS cod) == compose (getLHS dom) mapL) "Left square doesn't commute"
        , ensure (compose mapK (getRHS cod) == compose (getRHS dom) mapR) "Right square doesn't commute"
        ]

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
             (idMap (codomain (getLHS t)) (codomain (getLHS t)))
             (idMap (domain (getLHS t)) (domain (getLHS t)))
             (idMap (codomain (getRHS t)) (codomain (getRHS t)))

    isMonomorphism rm =
      isMonomorphism (mappingLeft rm) &&
      isMonomorphism (mappingInterface rm) &&
      isMonomorphism (mappingRight rm)

    isEpimorphism rm =
      isEpimorphism (mappingLeft rm) &&
      isEpimorphism (mappingInterface rm) &&
      isEpimorphism (mappingRight rm)

    isIsomorphism (RuleMorphism dom cod mapL mapK mapR) =
      isIsomorphism mapL &&
      isIsomorphism mapK &&
      isIsomorphism mapR &&
      compose (getLHS dom) mapL == compose mapK (getLHS cod) &&
      compose (getRHS dom) mapR == compose mapK (getRHS cod)


instance FindMorphism (RuleMorphism a b) where
  -- | A match between two first order rules (desconsidering the NACs)
  findMorphisms prop l g = map (buildPair l g) rightMatch
    where
      matchesK = findMorphisms prop (domain (getLHS l)) (domain (getLHS g))
      leftMatch = concatMap (leftM prop l g) matchesK
      rightMatch = concatMap (rightM prop l g) leftMatch

  partialInjectiveMatches n m =
    filter
      (\q ->
        isPartiallyMonomorphic (mappingLeft n) (mappingLeft q) &&
        isPartiallyMonomorphic (mappingInterface n) (mappingInterface q) &&
        isPartiallyMonomorphic (mappingRight n) (mappingRight q))
      (findAllMorphisms (codomain n) (codomain m))

leftM :: FindMorphism t => MorphismType -> Production t -> Production t -> t -> [(t, t)]
leftM prop l g mapK = map (\m -> (m, mapK)) commuting
  where
    matchesL = findMorphisms prop (codomain (getLHS l)) (codomain (getLHS g))
    commuting = filter (\m -> compose (getLHS l) m == compose mapK (getLHS g)) matchesL

-- commutes right side
rightM :: FindMorphism t =>  MorphismType -> Production t -> Production t -> (t, t) -> [(t, t, t)]
rightM prop l g (mapL,mapK) = map (\m -> (mapL, mapK, m)) commuting
  where
    matchesR = findMorphisms prop (codomain (getRHS l)) (codomain (getRHS g))
    commuting = filter (\m -> compose (getRHS l) m == compose mapK (getRHS g)) matchesR

-- kind of curry for three arguments
buildPair :: Production (TypedGraphMorphism a b)
        -> Production (TypedGraphMorphism a b)
        -> (TypedGraphMorphism a b,
            TypedGraphMorphism a b,
            TypedGraphMorphism a b)
        -> RuleMorphism a b
buildPair l g (m1,m2,m3) = ruleMorphism l g m1 m2 m3


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

satisfiesNACRewriting :: DPO m => m -> m -> Bool
satisfiesNACRewriting l = satisfiesGluingConditions dpoConf prod
  where
    -- Production just to test satisfiesGluingConditions, note that right side is not used.
    prod = buildProduction l l []
    dpoConf = DPOConfig AnyMatches MonomorphicNAC

instance AdhesiveHLR (RuleMorphism a b) where
  
  calculateInitialPushout _ = error "calculateInitialPushout for second order not implemented yet"
  
  calculatePushoutComplement (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK _ leftL leftK leftR) = (k,l')
     where
       (matchL', leftL') = calculatePushoutComplement matchL leftL
       (matchK', leftK') = calculatePushoutComplement matchK leftK
       (matchR', leftR') = calculatePushoutComplement matchR leftR
       l = commutingMorphismSameCodomain
             (compose leftK' (getLHS ruleG)) leftL'
             matchK' (compose (getLHS ruleK) matchL')
       r = commutingMorphismSameCodomain
             (compose leftK' (getRHS ruleG)) leftR'
             matchK' (compose (getRHS ruleK) matchR')

       validNACs = filter (satisfiesNACRewriting leftL') (getNACs ruleG)
       newRuleNACs = map (\nac -> fst (calculatePushoutComplement nac leftL')) validNACs

       newRule = buildProduction l r newRuleNACs
       k = RuleMorphism ruleK newRule matchL' matchK' matchR'
       l' = RuleMorphism newRule ruleG leftL' leftK' leftR'

  calculatePushout (RuleMorphism _ ruleD matchL matchK matchR) (RuleMorphism _ ruleR rightL rightK rightR) =  (m',r')
     where
       (matchL', rightL') = calculatePushout matchL rightL
       (matchK', rightK') = calculatePushout matchK rightK
       (matchR', rightR') = calculatePushout matchR rightR
       l = commutingMorphismSameDomain
             rightK' (compose (getLHS ruleD) rightL')
             matchK' (compose (getLHS ruleR) matchL')
       r = commutingMorphismSameDomain
             rightK' (compose (getRHS ruleD) rightR')
             matchK' (compose (getRHS ruleR) matchR')

       newRuleNACs = map (\nac -> fst (calculatePushout nac rightL')) (getNACs ruleD)

       newRule = buildProduction l r newRuleNACs
       m' = RuleMorphism ruleR newRule matchL' matchK' matchR'
       r' = RuleMorphism ruleD newRule rightL' rightK' rightR'

  -- TODO
  monomorphicPullback _ _ = error "monomorphicPullback not implemented in RuleMorphism"

  hasPushoutComplement (restrictionG, g) (restrictionF, f) =
    hasPushoutComplement (restrictionG, mappingLeft g) (restrictionF, mappingLeft f)
    && hasPushoutComplement (restrictionG, mappingRight g) (restrictionF, mappingRight f)
    && hasPushoutComplement (restrictionG, mappingInterface g) (restrictionF, mappingInterface f)
    && danglingSpan (getLHS $ codomain g) (mappingLeft g) (mappingInterface g) (mappingLeft f) (mappingInterface f)
    && danglingSpan (getRHS $ codomain g) (mappingRight g) (mappingInterface g) (mappingRight f) (mappingInterface f)

-- | A gluing condition for pushout complements of rule morphisms
danglingSpan :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
danglingSpan matchRuleSide matchMorp matchK l k = deletedNodesInK && deletedEdgesInK
  where
    deletedNodes = filter (checkDeletion l matchMorp applyNode nodesFromDomain) (nodesFromCodomain matchMorp)
    nodesInK = [a | a <- nodesFromDomain matchRuleSide, applyNodeUnsafe matchRuleSide a `elem` deletedNodes]
    deletedNodesInK = all (checkDeletion k matchK applyNode nodesFromDomain) nodesInK

    deletedEdges = filter (checkDeletion l matchMorp applyEdge edgesFromDomain) (edgesFromCodomain matchMorp)
    edgesInK = [a | a <- edgesFromDomain matchRuleSide, applyEdgeUnsafe matchRuleSide a `elem` deletedEdges]
    deletedEdgesInK = all (checkDeletion k matchK applyEdge edgesFromDomain) edgesInK


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
commutingMorphismSameDomain k1 s1 k2 s2 = buildTypedGraphMorphism (codomain k1) (codomain s1) select
  where
    mats = findMonomorphisms (codomain k1) (codomain s1)
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
commutingMorphismSameCodomain k1 s1 k2 s2 = buildTypedGraphMorphism (domain k1) (domain s1) select
  where
    mats = findMonomorphisms (domain k1) (domain s1)
    filt = filter (\m -> compose m s1 == k1 && compose k2 m == s2) mats
    select = case filt of
                [] -> error "(domain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(domain) Error when commuting monomorphic morphisms (non unique commuting morphism)"
