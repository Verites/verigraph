module Category.TypedGraphRule.Adhesive (createSideRule) where

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Category.TypedGraph.Adhesive               
import           Category.TypedGraph.CommutingSquares
import           Category.TypedGraphRule.Category
import           Category.TypedGraphRule.FindMorphism         ()
import           Category.TypedGraphRule.Finitary             ()
import           Category.TypedGraphRule.Limit                ()
import qualified Data.Graphs.Morphism                         as GM
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph                     ()
import           Rewriting.DPO.TypedGraphRule.NacManipulation

instance MInitialPushout (RuleMorphism a b) where

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
  calculateMInitialPushout f@(RuleMorphism fA fA' fL fK fR) = (b,d,c)
    where
      nodeTypesInAL = GM.applyNodeIdUnsafe (domain fL)
      edgeTypesInAL = GM.applyEdgeIdUnsafe (domain fL)
      nodeTypesInAR = GM.applyNodeIdUnsafe (domain fR)
      edgeTypesInAR = GM.applyEdgeIdUnsafe (domain fR)

      (initBL, _, _) = calculateMInitialPushout fL
      (bK, _, _) = calculateMInitialPushout fK
      (initBR, _, _) = calculateMInitialPushout fR

      nodesBL = [n | n <- nodeIds $ domain fL, isOrphanNode (leftMorphism fA) n, not (isOrphanNode (leftMorphism fA') (applyNodeIdUnsafe fL n))]
      edgesBL = [e | (e, _) <- edges $ domain fL, isOrphanEdge (leftMorphism fA) (edgeId e), not (isOrphanEdge (leftMorphism fA') (applyEdgeIdUnsafe fL (edgeId e)))]

      nodesBR = [n | n <- nodeIds $ domain fR, isOrphanNode (rightMorphism fA) n, not (isOrphanNode (rightMorphism fA') (applyNodeIdUnsafe fR n))]
      edgesBR = [e | (e, _) <- edges $ domain fR, isOrphanEdge (rightMorphism fA) (edgeId e), not (isOrphanEdge (rightMorphism fA') (applyEdgeIdUnsafe fR (edgeId e)))]

      prebL = foldr (\n -> createNodeOnDomain n (nodeTypesInAL n) n) initBL nodesBL
      bL = foldr (\e -> createEdgeOnDomain (edgeId e) (sourceId e) (targetId e) (edgeTypesInAL (edgeId e)) (edgeId e)) prebL edgesBL

      prebR = foldr (\n -> createNodeOnDomain n (nodeTypesInAR n) n) initBR nodesBR
      bR = foldr (\e -> createEdgeOnDomain (edgeId e) (sourceId e) (targetId e) (edgeTypesInAR (edgeId e)) (edgeId e)) prebR edgesBR

      l = searchMorphism (leftMorphism fA <&> bK) bL
      r = searchMorphism (rightMorphism fA <&> bK) bR
      searchMorphism a b = commutingMorphism a b a b

      ruleB = Production l r []
      b = RuleMorphism ruleB fA bL bK bR

      (d,c) = calculatePushoutComplementAlongM b f


instance MAdhesive (RuleMorphism a b) where

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
  calculatePushoutAlongM f@(RuleMorphism _ ruleD _ _ _) g@(RuleMorphism _ ruleR _ _ _) = (f',g')
    where
      (RuleMorphism _ preRuleP f'L f'K f'R,RuleMorphism _ _ g'L g'K g'R) =
        calculatePushout f g

      ruleP = Production (leftMorphism preRuleP) (rightMorphism preRuleP) nacsToAdd

      f' = RuleMorphism ruleR ruleP f'L f'K f'R
      g' = RuleMorphism ruleD ruleP g'L g'K g'R

      transposedNACs = createStep ShiftNACs g'L (nacs ruleD)

      createdNACs = createStep ShiftNACs f'L (nacs ruleR)

      nacsToAdd = transposedNACs ++ createdNACs

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
  calculatePushoutComplementAlongM (RuleMorphism ruleK ruleL leftL leftK leftR) (RuleMorphism _ ruleG matchL matchK matchR) = (k,l')
     where
       (matchL', leftL') = calculatePushoutComplementAlongM leftL matchL
       (matchK', leftK') = calculatePushoutComplementAlongM leftK matchK
       (matchR', leftR') = calculatePushoutComplementAlongM leftR matchR
       leftH = commutingMorphismSameCodomain
             (leftMorphism ruleG <&> leftK') leftL'
             matchK' (matchL' <&> leftMorphism ruleK)
       rightH = commutingMorphismSameCodomain
             (rightMorphism ruleG <&> leftK') leftR'
             matchK' (matchR' <&> rightMorphism ruleK)

       notDeletedNACs = deleteStep InitialPushouts (nacs ruleL) (nacs ruleG)

       validNACs = filter (satisfiesNACRewriting leftL') notDeletedNACs

       newRuleNACs = map (fst . calculatePushoutComplementAlongM leftL') validNACs

       ruleH = Production leftH rightH newRuleNACs
       k = RuleMorphism ruleK ruleH matchL' matchK' matchR'
       l' = RuleMorphism ruleH ruleG leftL' leftK' leftR'

  hasPushoutComplementAlongM f g =
    hasPushoutComplementAlongM (mappingLeft f) (mappingLeft g)
    && hasPushoutComplementAlongM (mappingRight f) (mappingRight g)
    && hasPushoutComplementAlongM (mappingInterface f) (mappingInterface g)
    && danglingSpan (leftMorphism $ codomain g) (mappingLeft g) (mappingInterface g) (mappingLeft f) (mappingInterface f)
    && danglingSpan (rightMorphism $ codomain g) (mappingRight g) (mappingInterface g) (mappingRight f) (mappingInterface f)

-- | A gluing condition for pushout complements of rule morphisms
danglingSpan :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
danglingSpan matchRuleSide matchMorp matchK l k = deletedNodesInK && deletedEdgesInK
  where
    deletedNodes = filter (isDeleted l matchMorp applyNodeId (nodeIds . domain)) (nodeIds $ codomain matchMorp)
    nodesInK = [a | a <- nodeIds $ domain matchRuleSide, applyNodeIdUnsafe matchRuleSide a `elem` deletedNodes]
    deletedNodesInK = all (isDeleted k matchK applyNodeId (nodeIds . domain)) nodesInK

    deletedEdges = filter (isDeleted l matchMorp applyEdgeId (edgeIds . domain)) (edgeIds $ codomain matchMorp)
    edgesInK = [a | a <- edgeIds $ domain matchRuleSide, applyEdgeIdUnsafe matchRuleSide a `elem` deletedEdges]
    deletedEdgesInK = all (isDeleted k matchK applyEdgeId (edgeIds . domain)) edgesInK

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodeIds m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m e = e `elem` orphanTypedEdgeIds m


instance E'PairCofinitary (RuleMorphism n e) where
  
  isJointSurjection (RuleMorphism _ _ fL fK fR, RuleMorphism _ _ gL gK gR) =
    isJointSurjection (fL, gL) && isJointSurjection (fK, gK) && isJointSurjection (fR, gR)

  findJointSurjections (cls1', p1) (cls2', p2) = ret
    where
      (cls1, cls2) = (toFstOrderMorphismClass cls1', toFstOrderMorphismClass cls2')
      createJointly x y = findJointSurjections (cls1, codomain x) (cls2, codomain y)

      leftM1 = leftMorphism p1
      rightM1 = rightMorphism p1
      leftM2 = leftMorphism p2
      rightM2 = rightMorphism p2
      k1 = domain leftM1
      k2 = domain leftM2

      ks = findJointSurjections (cls1, k1) (cls2, k2)

      lefts = concatMap
                (\(k1,k2) -> let ls = createSideRule createJointly k1 leftM1 leftM1 k2 leftM2 leftM2
                              in map (\(ll1,ll2,m) -> (k1, k2, ll1, ll2, m)) ls) ks
      rights = concatMap
                (\(k1,k2,ll1,ll2,l) -> let rs = createSideRule createJointly k1 rightM1 rightM1 k2 rightM2 rightM2
                                        in map (\(rr1,rr2,r) -> (k1,k2,ll1,ll2,l,rr1,rr2,r)) rs) lefts

      transposeNACs l = map (snd . calculatePushoutAlongM l)

      ret = map (\(k1,k2,ll1,ll2,l,r1,r2,r) ->
                    let rule = Production l r $ transposeNACs ll1 (nacs p1) ++ transposeNACs ll2 (nacs p2)
                    in (ruleMorphism p1 rule ll1 k1 r1,
                        ruleMorphism p2 rule ll2 k2 r2)) rights
  
-- | Generates all (ss1,ss2,m) morphisms that commute with all JointlyEpimorphisms
-- of S1 and S2.
-- Morphism morph is always monomorphic.
-- createS must create all ss1 and ss2 from create1 and create2.
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
createSideRule :: (TypedGraphMorphism a b -> TypedGraphMorphism a b -> [(TypedGraphMorphism a b, TypedGraphMorphism a b)])
            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
            -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
            -> [(TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b)]
createSideRule createS k1 sideM1 create1 k2 sideM2 create2 = d
  where
    a = createS create1 create2
    b = concatMap (\(ss1,ss2) -> sequence [[ss1],[ss2], findMonomorphisms (codomain k1) (codomain ss1)]) a
    c = map (\(x:y:z:_) -> (x,y,z)) b
    d = filter (\(ss1,ss2,m) -> ss1 <&> sideM1 == m <&> k1 &&
                                ss2 <&> sideM2 == m <&> k2) c
  
