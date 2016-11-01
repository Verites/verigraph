{-# OPTIONS_GHC -fno-warn-orphans #-}

module SndOrder.Morphism.AdhesiveHLR where

import           Abstract.AdhesiveHLR
import           Abstract.Cocomplete
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.Graph              as G
import qualified Graph.GraphMorphism      as GM
import           TypedGraph.Morphism

import           SndOrder.Morphism.Cocomplete ()
import           SndOrder.Morphism.CommutingSquares
import           SndOrder.Morphism.Core

instance AdhesiveHLR (RuleMorphism a b) where

  -- Pushout for second-order with creation of NACs.
  -- It runs the pushout without NACs (from cocomplete),
  -- generates all NACs (considering arbitrary matches) for the rule H,
  -- and updates the morphisms f and g to get the new NACs.
  --
  -- @
  --       g
  --    K──────▶R
  --    │       │
  --  f │       │ f'
  --    ▼       ▼
  --    D──────▶H
  --       g'
  -- @
  calculatePushout f@(RuleMorphism _ ruleD _ _ _) g@(RuleMorphism _ ruleR _ _ _) = (f',g')
    where
      (RuleMorphism _ ruleH f'L f'K f'R,RuleMorphism _ _ g'L g'K g'R) =
        Abstract.Cocomplete.calculatePushout f g
      
      ruleHwithNACs = buildProduction (getLHS ruleH) (getRHS ruleH) nacsToAdd
      
      f' = RuleMorphism ruleR ruleHwithNACs f'L f'K f'R
      g' = RuleMorphism ruleD ruleHwithNACs g'L g'K g'R
      
      nacsToAdd = newNACs
        where
          transposedNACs = map (\nac -> fst (Abstract.Cocomplete.calculatePushout nac g'L)) (getNACs ruleD)
          
          -- conf is used only to indicate AnyMatches, that is the most generic case for nacDownwardShift
          conf = DPOConfig AnyMatches MonomorphicNAC
          createdNACs = concatMap (nacDownwardShift conf f'L) (getNACs ruleR)
          
          -- The new NACs are the transposed and the created that do not are included on the transposed
          newNACs =
            transposedNACs ++
            (filter (\n -> all (\n' -> Prelude.null (findIso n n')) transposedNACs) createdNACs)
              where
                findIso :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
                findIso a b = findMorphisms Isomorphism (domain a) (domain b)

  -- @
  --        d
  --    B──────▶C
  --    │       │
  --  b │  (1)  │ c
  --    ▼       ▼
  --    A──────▶A'
  --        f
  -- @
  --
  -- This function for second-order must run the first-order initial
  -- pushouts and after add elements to the boundary (B) rule if
  -- it was generated with dangling span condition
  calculateInitialPushout f@(RuleMorphism fA fA' fL fK fR) = (b,d,c)
    where
      nodeTypesInAL = GM.applyNodeUnsafe (domain fL)
      edgeTypesInAL = GM.applyEdgeUnsafe (domain fL)
      graphAL = domain (domain fL)
      nodeTypesInAR = GM.applyNodeUnsafe (domain fR)
      edgeTypesInAR = GM.applyEdgeUnsafe (domain fR)
      graphAR = domain (domain fR)
      
      (initBL, _, _) = calculateInitialPushout fL
      (bK, _, _) = calculateInitialPushout fK
      (initBR, _, _) = calculateInitialPushout fR
      
      nodesBL = [n | n <- nodesFromDomain fL, isOrphanNode (getLHS fA) n, not (isOrphanNode (getLHS fA') (applyNodeUnsafe fL n))]
      edgesBL = [e | e <- edgesFromDomain fL, isOrphanEdge (getLHS fA) e, not (isOrphanEdge (getLHS fA') (applyEdgeUnsafe fL e))]
      
      nodesBR = [n | n <- nodesFromDomain fR, isOrphanNode (getRHS fA) n, not (isOrphanNode (getRHS fA') (applyNodeUnsafe fR n))]
      edgesBR = [e | e <- edgesFromDomain fR, isOrphanEdge (getRHS fA) e, not (isOrphanEdge (getRHS fA') (applyEdgeUnsafe fR e))]
      
      prebL = foldr (\n -> createNodeOnDomain n (nodeTypesInAL n) n) initBL nodesBL
      bL = foldr (\e -> createEdgeOnDomain e (src e) (tgt e) (edgeTypesInAL e) e) prebL edgesBL
        where
          src = sourceOfUnsafe graphAL
          tgt = targetOfUnsafe graphAL
      
      prebR = foldr (\n -> createNodeOnDomain n (nodeTypesInAR n) n) initBR nodesBR
      bR = foldr (\e -> createEdgeOnDomain e (src e) (tgt e) (edgeTypesInAR e) e) prebR edgesBR
        where
          src = sourceOfUnsafe graphAR
          tgt = targetOfUnsafe graphAR
      
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
  
  -- @
  --        g'
  --     X──────▶A
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     B──────▶C
  --        g
  -- @
  calculatePullback (RuleMorphism fA _ fL fK fR) (RuleMorphism gB _ gL gK gR) = (f',g')
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
      f' = RuleMorphism x gB f'L f'K f'R
      g' = RuleMorphism x fA g'L g'K g'R
  
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

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodes m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m e = e `elem` orphanTypedEdges m
