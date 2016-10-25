{-# OPTIONS_GHC -fno-warn-orphans #-}

module SndOrder.Morphism.AdhesiveHLR where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           TypedGraph.Morphism

import           SndOrder.Morphism.Core

instance AdhesiveHLR (RuleMorphism a b) where
  
  -- TODO
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
  calculatePullback _ _ = error "calculatePullback not implemented in second order"

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
