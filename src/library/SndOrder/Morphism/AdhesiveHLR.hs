module SndOrder.Morphism.AdhesiveHLR where

import           Abstract.Category.AdhesiveHLR
import           Abstract.Category.Cocomplete
import           Abstract.Category.DPO
import           Abstract.Category.FinitaryCategory          ()
import           Data.Graphs                        as G
import qualified Morphism.Graph                as GM
import           TypedGraph.Morphism

import           SndOrder.Morphism.Cocomplete       ()
import           SndOrder.Morphism.CommutingSquares
import           SndOrder.Morphism.Core
import           SndOrder.Morphism.NACmanipulation

instance AdhesiveHLR (RuleMorphism a b) where

  -- Pushout for second-order with creation of NACs.
  -- It runs the pushout without NACs (from cocomplete),
  -- generates all NACs (considering arbitrary matches) for the rule P,
  -- and updates the morphisms f and g to get the new NACs.
  --
  -- @
  --       g
  --    K──────▶R
  --    │       │
  --  f │       │ f'
  --    ▼       ▼
  --    D──────▶P
  --       g'
  -- @
  calculatePushout f@(RuleMorphism _ ruleD _ _ _) g@(RuleMorphism _ ruleR _ _ _) = (f',g')
    where
      (RuleMorphism _ preRuleP f'L f'K f'R,RuleMorphism _ _ g'L g'K g'R) =
        Abstract.Category.Cocomplete.calculatePushout f g

      ruleP = buildProduction (getLHS preRuleP) (getRHS preRuleP) nacsToAdd

      f' = RuleMorphism ruleR ruleP f'L f'K f'R
      g' = RuleMorphism ruleD ruleP g'L g'K g'R

      transposedNACs = map (\nac -> fst (Abstract.Category.Cocomplete.calculatePushout nac g'L)) (getNACs ruleD)

      createdNACs = createStep ShiftNACs f'L (getNACs ruleR)

      nacsToAdd = transposedNACs ++ createdNACs

  -- This function for second-order must run the first-order initial
  -- pushouts and after add elements to the boundary (B) rule if
  -- it was generated with dangling span condition
  --
  -- @
  --        d
  --    B──────▶C
  --    │       │
  --  b │  (1)  │ c
  --    ▼       ▼
  --    A──────▶A'
  --        f
  -- @
  calculateInitialPushout f@(RuleMorphism fA fA' fL fK fR) = (b,d,c)
    where
      nodeTypesInAL = GM.applyNodeIdUnsafe (domain fL)
      edgeTypesInAL = GM.applyEdgeIdUnsafe (domain fL)
      nodeTypesInAR = GM.applyNodeIdUnsafe (domain fR)
      edgeTypesInAR = GM.applyEdgeIdUnsafe (domain fR)

      (initBL, _, _) = calculateInitialPushout fL
      (bK, _, _) = calculateInitialPushout fK
      (initBR, _, _) = calculateInitialPushout fR

      nodesBL = [n | n <- nodeIdsFromDomain fL, isOrphanNode (getLHS fA) n, not (isOrphanNode (getLHS fA') (applyNodeIdUnsafe fL n))]
      edgesBL = [e | e <- edgesFromDomain fL, isOrphanEdge (getLHS fA) (edgeId e), not (isOrphanEdge (getLHS fA') (applyEdgeIdUnsafe fL (edgeId e)))]

      nodesBR = [n | n <- nodeIdsFromDomain fR, isOrphanNode (getRHS fA) n, not (isOrphanNode (getRHS fA') (applyNodeIdUnsafe fR n))]
      edgesBR = [e | e <- edgesFromDomain fR, isOrphanEdge (getRHS fA) (edgeId e), not (isOrphanEdge (getRHS fA') (applyEdgeIdUnsafe fR (edgeId e)))]

      prebL = foldr (\n -> createNodeOnDomain n (nodeTypesInAL n) n) initBL nodesBL
      bL = foldr (\e -> createEdgeOnDomain (edgeId e) (sourceId e) (targetId e) (edgeTypesInAL (edgeId e)) (edgeId e)) prebL edgesBL

      prebR = foldr (\n -> createNodeOnDomain n (nodeTypesInAR n) n) initBR nodesBR
      bR = foldr (\e -> createEdgeOnDomain (edgeId e) (sourceId e) (targetId e) (edgeTypesInAR (edgeId e)) (edgeId e)) prebR edgesBR

      l = searchMorphism (getLHS fA <&> bK) bL
      r = searchMorphism (getRHS fA <&> bK) bR
      searchMorphism a b = commutingMorphism a b a b

      ruleB = buildProduction l r []
      b = RuleMorphism ruleB fA bL bK bR

      (d,c) = calculatePushoutComplement f b

  -- Pushout Complement for second-order with deletion and transposing of NACs.
  -- It runs the pushout complement without NACs,
  -- filters the NACs in the matched rule (ruleG) selecting the non deleted,
  -- and updates the rule H with the transposed NACs.
  --
  -- @
  --        l
  --    L◀──────K
  --    │       │
  --  m │       │ k
  --    ▼       ▼
  --    G◀──────H
  --        l'
  -- @
  -- calculatePushoutComplement morph l = (k,l')
  calculatePushoutComplement (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK ruleL leftL leftK leftR) = (k,l')
     where
       (matchL', leftL') = calculatePushoutComplement matchL leftL
       (matchK', leftK') = calculatePushoutComplement matchK leftK
       (matchR', leftR') = calculatePushoutComplement matchR leftR
       leftH = commutingMorphismSameCodomain
             (getLHS ruleG <&> leftK') leftL'
             matchK' (matchL' <&> getLHS ruleK)
       rightH = commutingMorphismSameCodomain
             (getRHS ruleG <&> leftK') leftR'
             matchK' (matchR' <&> getRHS ruleK)

       notDeletedNACs = deleteStep InitialPushouts (getNACs ruleL) (getNACs ruleG)

       validNACs = filter (satisfiesNACRewriting leftL') notDeletedNACs

       newRuleNACs = map (\nac -> fst (calculatePushoutComplement nac leftL')) validNACs

       ruleH = buildProduction leftH rightH newRuleNACs
       k = RuleMorphism ruleK ruleH matchL' matchK' matchR'
       l' = RuleMorphism ruleH ruleG leftL' leftK' leftR'

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
            (getLHS gB <&> f'K) f'L
            (getLHS fA <&> g'K) g'L

      r = commutingMorphism
            (getRHS gB <&> f'K) f'R
            (getRHS fA <&> g'K) g'R

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
    deletedNodes = filter (isDeleted l matchMorp applyNodeId nodeIdsFromDomain) (nodeIdsFromCodomain matchMorp)
    nodesInK = [a | a <- nodeIdsFromDomain matchRuleSide, applyNodeIdUnsafe matchRuleSide a `elem` deletedNodes]
    deletedNodesInK = all (isDeleted k matchK applyNodeId nodeIdsFromDomain) nodesInK

    deletedEdges = filter (isDeleted l matchMorp applyEdgeId edgeIdsFromDomain) (edgeIdsFromCodomain matchMorp)
    edgesInK = [a | a <- edgeIdsFromDomain matchRuleSide, applyEdgeIdUnsafe matchRuleSide a `elem` deletedEdges]
    deletedEdgesInK = all (isDeleted k matchK applyEdgeId edgeIdsFromDomain) edgesInK

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodeIds m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m e = e `elem` orphanTypedEdgeIds m
