{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}

module SndOrder.Morphism.AdhesiveHLR where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Morphism
import           TypedGraph.Morphism

import           SndOrder.Morphism.Core

instance AdhesiveHLR (RuleMorphism a b) where

  -- @
  --        d
  --    B──────▶C
  --    │       │
  --  b │  (1)  │ c
  --    ▼       ▼
  --    A──────▶A'
  --        f
  -- @
  calculateInitialPushout f@(RuleMorphism fA _ fL fK fR) = (b,d,c)
    where
      (bL, _, _) = calculateInitialPushout fL
      (bK, _, _) = calculateInitialPushout fK
      (bR, _, _) = calculateInitialPushout fR
      
      l = commutingMorphism
            (compose bK (getLHS fA)) bL
            (compose bK (getLHS fA)) bL
      
      r = commutingMorphism
            (compose bK (getRHS fA)) bR
            (compose bK (getRHS fA)) bR
      
      ruleB = buildProduction l r []
      b = RuleMorphism ruleB fA bL bK bR
      
      (d,c) = calculatePushoutComplement f b
  
  calculatePushoutComplement (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK ruleL leftL leftK leftR) = (k,l')
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
       
       notDeletedNACs = filter (\n -> all (\n' -> Prelude.null (findMorph n n')) (getNACs ruleL)) (getNACs ruleG)
         where
           findMorph :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
           findMorph a b = findMorphisms Monomorphism (domain a) (domain b)
       
       validNACs = filter (satisfiesNACRewriting leftL') notDeletedNACs
       
       newRuleNACs = map (\nac -> fst (calculatePushoutComplement nac leftL')) validNACs

       newRule = buildProduction l r newRuleNACs
       k = RuleMorphism ruleK newRule matchL' matchK' matchR'
       l' = RuleMorphism newRule ruleG leftL' leftK' leftR'

  calculatePushout (RuleMorphism _ ruleD matchL matchK matchR) (RuleMorphism _ ruleR rightL rightK rightR) = (m',r')
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

       transposedNACs = map (\nac -> fst (calculatePushout nac rightL')) (getNACs ruleD)
       
       -- conf is used only to indicate AnyMatches, that is the most generic case for nacDownwardShift
       conf = DPOConfig AnyMatches MonomorphicNAC
       createdNACs = concatMap (nacDownwardShift conf matchL') (getNACs ruleR) 
       
       -- The new NACs are the transposed and the created that do not are included on the transposed
       newNACs =
         transposedNACs ++
         (filter (\n -> all (\n' -> Prelude.null (findIso n n')) transposedNACs) createdNACs)
         where
           findIso :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
           findIso a b = findMorphisms Isomorphism (domain a) (domain b)
       
       newRule = buildProduction l r newNACs
       m' = RuleMorphism ruleR newRule matchL' matchK' matchR'
       r' = RuleMorphism ruleD newRule rightL' rightK' rightR'
  
  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  -- @
  calculatePullback (RuleMorphism fA _ fL fK fR) (RuleMorphism gB _ gL gK gR) = (g',f')
    where
      (f'L, g'L) = calculatePullback fL gL
      (f'K, g'K) = calculatePullback fK gK
      (f'R, g'R) = calculatePullback fR gR
      
      l = commutingMorphism
            (compose f'K (getLHS gB)) f'L
            (compose g'K (getLHS fA)) g'L
      
      r = commutingMorphism
            (compose f'K (getRHS gB)) f'R
            (compose g'K (getRHS fA)) g'R
      
      x = buildProduction l r []
      f' = RuleMorphism x gB g'L g'K g'R
      g' = RuleMorphism x fA f'L f'K f'R

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

-- | Given the morphisms /a1 : X -> B1/, /b1 : Y -> B1/,
-- /a2 : X -> B2/ and /b2 : Y -> B2/, respectively,
-- creates the monomorphic morphism /x : X -> Y/,
-- where the following diagram commutes:
--
-- @
--         X
--         |
--   a1    |x   a2
--         ▼
--  B1◀────Y────▶B2
--      b1   b2
-- @
--
-- TODO: explain the errors in this function. what are preconditions for them not to occur?!?
commutingMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b
                  -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphism a1 b1 a2 b2 = buildTypedGraphMorphism (domain a1) (domain b1) select
  where
    mats = findMonomorphisms (domain a1) (domain b1)
    filt = filter (\m -> compose m b1 == a1 && compose m b2 == a2) mats
    select = case filt of
                [] -> error "(domain) Error when commuting monomorphic morphisms (must be generating an invalid rule)"
                [x] -> mapping x
                (_:_:_) -> error "(domain) Error when commuting monomorphic morphisms (non unique commuting morphism)"

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
