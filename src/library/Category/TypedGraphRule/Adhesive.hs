module Category.TypedGraphRule.Adhesive where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Category.TypedGraph.CommutingSquares
import           Category.TypedGraphRule.Category
import           Category.TypedGraphRule.Limit                ()
import           Data.Graphs                                  as G
import qualified Data.Graphs.Morphism                         as GM
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph                     ()
import           Rewriting.DPO.TypedGraphRule.NacManipulation
import Util.Monad

instance Adhesive (TGRuleCat n e) (RuleMorphism n e) where

  -- Pushout for second-order with creation of NACs.
  -- It runs the pushout without NACs (from cocomplete),
  -- generates all NACs (considering arbitrary matches) for the rule P,
  -- and updates the morphisms f and g to get the new NACs.
  --
  -- @
  --       f
  --    Z──────▶Y
  --    │       │
  --  m │       │ m'
  --    ▼       ▼
  --    X──────▶S
  --       f'
  -- @

  -- FIXME: why is this different from `calculatePushout` from Cocomplete?
  calculatePushoutAlongMono m@(RuleMorphism _ ruleX _ _ _) f@(RuleMorphism _ ruleY _ _ _) = do 
    (RuleMorphism _ preRuleS m'L m'K m'R, RuleMorphism _ _ f'L f'K f'R) <- calculatePushout m f
    liftTGraph $ do
      transposedNACs <- mapM (\nac -> fst <$> calculatePushoutAlongMono nac m'L) (nacs ruleY)
      createdNACs <- createStep ShiftNACs f'L (nacs ruleX)
      let
        ruleS = preRuleS { nacs = transposedNACs ++ createdNACs }
        m' = RuleMorphism ruleY ruleS m'L m'K m'R
        f' = RuleMorphism ruleX ruleS f'L f'K f'R
      return (m', f')


  -- Pushout Complement for second-order with deletion and transposing of NACs.
  -- It runs the pushout complement without NACs,
  -- filters the NACs in the matched rule (ruleG) selecting the non deleted,
  -- and updates the rule H with the transposed NACs.
  --
  -- @
  --        f
  --     A──────▶B
  --     │       │
  --  g' │       │ g
  --     ▼       ▼
  --     X──────▶C
  --        f'
  -- @
  calculatePushoutComplementAlongMono (RuleMorphism ruleA ruleB fL fK fR) (RuleMorphism _ ruleC gL gK gR) = liftTGraph $ do
    (gL', fL') <- calculatePushoutComplementAlongMono fL gL
    (gK', fK') <- calculatePushoutComplementAlongMono fK gK
    (gR', fR') <- calculatePushoutComplementAlongMono fR gR

    {- Find monomorphism /lX : KX->LX/ making the following commute
           gL'      fL'    
        LA─────▶LX─────▶LC
        ▲        ▲       ▲  
      lA│      lX│     lC│  
        │        │       │  
        KA─────▶KX─────▶KC  
           gK'      fK'    -}
    leftX <- commutingMorphismSameCodomain
          (leftMorphism ruleC <&> fK') fL'
          gK' (gL' <&> leftMorphism ruleA)
    rightX <- commutingMorphismSameCodomain
          (rightMorphism ruleC <&> fK') fR'
          gK' (gR' <&> rightMorphism ruleA)

    notDeletedNACs <- deleteStep InitialPushouts (nacs ruleB) (nacs ruleC)
    validNACs <- filterM (satisfiesNACRewriting fL') notDeletedNACs

    newRuleNACs <- mapM (\nac -> fst <$> calculatePushoutComplementAlongMono nac fL') validNACs

    let
      ruleX = buildProduction leftX rightX newRuleNACs
      g' = RuleMorphism ruleA ruleX gL' gK' gR'
      f' = RuleMorphism ruleX ruleC fL' fK' fR'
    return (g', f')

  hasPushoutComplementAlongMono f g = liftTGraph $
      hasPushoutComplementAlongMono (mappingLeft f) (mappingLeft g)
      `andM` hasPushoutComplementAlongMono (mappingRight f) (mappingRight g)
      `andM` hasPushoutComplementAlongMono (mappingInterface f) (mappingInterface g)
      `andM` return 
        (danglingSpan (leftMorphism $ codomain g) (mappingLeft g) (mappingInterface g) (mappingLeft f) (mappingInterface f)
        && danglingSpan (rightMorphism $ codomain g) (mappingRight g) (mappingInterface g) (mappingRight f) (mappingInterface f))

instance LRNAdhesive (TGRuleCat n e) (RuleMorphism n e) where
  ruleMorphism = monic
  leftHandMorphism = monic
  matchMorphism = monic -- FIXME: do we allow arbitrary snd-order matches?

  hasPushoutComplementOfRN = hasPushoutComplementAlongMono
  calculatePushoutComplementOfRN = calculatePushoutComplementAlongMono

instance InitialPushout (TGRuleCat n e) (RuleMorphism n e) where
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
  --    X──────▶Y
  --        f
  -- @
  calculateInitialPushout f@(RuleMorphism ruleX ruleY fL fK fR) = do 
    let
      nodeTypesInAL = GM.applyNodeIdUnsafe (domain fL)
      edgeTypesInAL = GM.applyEdgeIdUnsafe (domain fL)
      nodeTypesInAR = GM.applyNodeIdUnsafe (domain fR)
      edgeTypesInAR = GM.applyEdgeIdUnsafe (domain fR)

    (initBL, _, _) <- liftTGraph $ calculateInitialPushout fL
    (bK, _, _) <- liftTGraph $ calculateInitialPushout fK
    (initBR, _, _) <- liftTGraph $ calculateInitialPushout fR

    let
      nodesBL = [n | n <- nodeIdsFromDomain fL, isOrphanNode (leftMorphism ruleX) n, not (isOrphanNode (leftMorphism ruleY) (applyNodeIdUnsafe fL n))]
      edgesBL = [e | e <- edgesFromDomain fL, isOrphanEdge (leftMorphism ruleX) (edgeId e), not (isOrphanEdge (leftMorphism ruleY) (applyEdgeIdUnsafe fL (edgeId e)))]

      nodesBR = [n | n <- nodeIdsFromDomain fR, isOrphanNode (rightMorphism ruleX) n, not (isOrphanNode (rightMorphism ruleY) (applyNodeIdUnsafe fR n))]
      edgesBR = [e | e <- edgesFromDomain fR, isOrphanEdge (rightMorphism ruleX) (edgeId e), not (isOrphanEdge (rightMorphism ruleY) (applyEdgeIdUnsafe fR (edgeId e)))]

      prebL = foldr (\n -> createNodeOnDomain n (nodeTypesInAL n) n) initBL nodesBL
      bL = foldr (\e -> createEdgeOnDomain (edgeId e) (sourceId e) (targetId e) (edgeTypesInAL (edgeId e)) (edgeId e)) prebL edgesBL

      prebR = foldr (\n -> createNodeOnDomain n (nodeTypesInAR n) n) initBR nodesBR
      bR = foldr (\e -> createEdgeOnDomain (edgeId e) (sourceId e) (targetId e) (edgeTypesInAR (edgeId e)) (edgeId e)) prebR edgesBR

    lB <- liftTGraph $ searchMorphism (leftMorphism ruleX <&> bK) bL
    rB <- liftTGraph $ searchMorphism (rightMorphism ruleX <&> bK) bR
    let
      ruleB = buildProduction lB rB []
      b = RuleMorphism ruleB ruleX bL bK bR

    (d,c) <- calculatePushoutComplementAlongMono b f
    return (b,d,c)
    where
      searchMorphism a b = commutingMorphism a b a b


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

satisfiesNACRewriting :: DPO cat morph => morph -> morph -> cat Bool
satisfiesNACRewriting l = satisfiesGluingConditions (buildProduction l undefined [])

-- | Given the left-hand-side morphism of a rule /l : K -> L/, a match /m : L -> G/ of this rule, an element __/e/__
-- (that can be either a __/Node/__ or an __/Edge/__) and two functions /apply/ (for applying that element in a TypedGraphMorphism) and
-- /list/ (to get all the corresponding elements in the domain of m), it returns true if /e/ is deleted by this rule for the given match
isDeleted :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> t -> Maybe t)
          -> (TypedGraphMorphism a b -> [t]) -> t -> Bool
isDeleted l m apply list e = elementInL && not elementInK
  where
    elementInL = any (\x -> apply m x == Just e) (list m)
    kToG = m <&> l
    elementInK = any (\x -> apply kToG x == Just e) (list kToG)
