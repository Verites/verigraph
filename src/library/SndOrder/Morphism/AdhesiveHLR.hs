module SndOrder.Morphism.AdhesiveHLR where

import           Abstract.AdhesiveHLR
import           Abstract.Cocomplete
import           Abstract.DPO
import           Abstract.Morphism
import           Graph.Graph                        as G
import qualified Graph.GraphMorphism                as GM
import           TypedGraph.Morphism

import           SndOrder.Morphism.Cocomplete       ()
import           SndOrder.Morphism.CommutingSquares
import           SndOrder.Morphism.Core

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
        Abstract.Cocomplete.calculatePushout f g

      ruleP = buildProduction (getLHS preRuleP) (getRHS preRuleP) nacsToAdd

      f' = RuleMorphism ruleR ruleP f'L f'K f'R
      g' = RuleMorphism ruleD ruleP g'L g'K g'R
      
      transposedNACs = map (\nac -> fst (Abstract.Cocomplete.calculatePushout nac g'L)) (getNACs ruleD)

      createdNACs = createStep DisableCreate f'L (getNACs ruleR)

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

      l = searchMorphism (compose bK (getLHS fA)) bL
      r = searchMorphism (compose bK (getRHS fA)) bR
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
  -- calculatePushoutComplement m l = (k,l')
  calculatePushoutComplement (RuleMorphism _ ruleG matchL matchK matchR) (RuleMorphism ruleK ruleL leftL leftK leftR) = (k,l')
     where
       (matchL', leftL') = calculatePushoutComplement matchL leftL
       (matchK', leftK') = calculatePushoutComplement matchK leftK
       (matchR', leftR') = calculatePushoutComplement matchR leftR
       leftH = commutingMorphismSameCodomain
             (compose leftK' (getLHS ruleG)) leftL'
             matchK' (compose (getLHS ruleK) matchL')
       rightH = commutingMorphismSameCodomain
             (compose leftK' (getRHS ruleG)) leftR'
             matchK' (compose (getRHS ruleK) matchR')

       notDeletedNACs = deleteStep InitialPushouts matchL (getNACs ruleL) (getNACs ruleG)

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

-- | Auxiliar structure and function to delete first-order NACs
data DeleteScheme = DisableDelete | Monomorphisms | InitialPushouts

deleteStep :: DeleteScheme -> TypedGraphMorphism a b -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b]

deleteStep DisableDelete _ _ concreteNACs = concreteNACs

deleteStep Monomorphisms _ modeledNACs concreteNACs =
  [nn' | nn' <- concreteNACs, all (\nn -> maintainTest nn nn') modeledNACs]
    where
      findMorph :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
      findMorph a b = findMorphisms Monomorphism (codomain a) (codomain b)
      
      --it forces commuting 
      --maintainTest a b = Prelude.null $ filter (\morp -> compose a morp == compose match b) (findMorph a b)
      maintainTest a b = Prelude.null $ findMorph a b

deleteStep InitialPushouts _ modeledNACs concreteNACs =
  [fst nn' | nn' <- ipoConcrete, all (\nn -> verifyIsoBetweenMorphisms nn (snd nn')) ipoModeled]
  where
    ipoModeled = map (\(_,x,_) -> x) (map calculateInitialPushout modeledNACs)
    ipoConcrete = map (\(n,(_,x,_)) -> (n,x)) (zip concreteNACs (map calculateInitialPushout concreteNACs))

verifyIsoBetweenMorphisms :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
verifyIsoBetweenMorphisms n n' = Prelude.null comb
  where
    findIsoDom = findIso domain n n'
    findIsoCod = findIso codomain n n'
    comb = [(d,c) | d <- findIsoDom, c <- findIsoCod, compose d n' == compose n c]

findIso :: (TypedGraphMorphism a b -> GM.GraphMorphism a b) -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
findIso f x y = findMorphisms Isomorphism (f x) (f y)

-- | Auxiliar structure and function to create first-order NACs
data CreateScheme = DisableCreate | Pushout | ShiftNACs

createStep :: CreateScheme -> TypedGraphMorphism a b -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b]

createStep DisableCreate _ _ = []

createStep Pushout match modeledNACs =
  map snd $ map (Abstract.AdhesiveHLR.calculatePushout match) modeledNACs

createStep ShiftNACs match modeledNACs =
  concatMap (nacDownwardShift conf match) modeledNACs
    where
      -- conf is used only to indicate AnyMatches, that is the most generic case for nacDownwardShift
      conf = MorphismsConfig AnyMatches MonomorphicNAC

isOrphanNode :: TypedGraphMorphism a b -> NodeId -> Bool
isOrphanNode m n = n `elem` orphanTypedNodes m

isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m e = e `elem` orphanTypedEdges m
