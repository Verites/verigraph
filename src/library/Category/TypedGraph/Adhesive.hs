module Category.TypedGraph.Adhesive () where

import           Data.Maybe                         (fromJust, mapMaybe)

import           Abstract.Category.NewClasses
import           Category.TypedGraph.Category       
import           Category.TypedGraph.Limit          ()
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph.Morphism

instance Adhesive (CatM n e) (TypedGraphMorphism n e) where

  {-
     PO complement algorithm:
     1. compose l and m generating ml
     2. query edges for deletion in the codomain of ml
     2. query nodes for deletion in the codomain of ml
     3. delete all edges
     4. delete all nodes
  -}
  calculatePushoutComplementAlongMono l m = return $
    let ml       = m <&> l                                                -- compose l and m obtaining ml
        delEdges = mapMaybe (GM.applyEdgeId $ mapping m) (orphanTypedEdgeIds l) -- obtain list of edges to be deleted in G
        delNodes = mapMaybe (GM.applyNodeId $ mapping m) (orphanTypedNodeIds l) -- obtain list of nodes to be deleted in G
        k        = foldr removeNodeFromCodomain                               -- delete all edges, then all nodes from ml
                       (foldr removeEdgeFromCodomain ml delEdges)
                           delNodes
    in (k, idMap (codomain k) (codomain m))

  hasPushoutComplementAlongMono f g = return $ 
    satisfiesDanglingCondition f g && satisfiesIdentificationCondition f g

-- TODO: remove unused functions below?
generateNewNodeInstances :: TypedGraphMorphism a b -> [(NodeId, NodeId)] -> TypedGraphMorphism a b
generateNewNodeInstances gf =
  foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNodeId (domain gf) a
                       in updateNodeRelation a b tp tgm) gf


generateNewEdgeInstances :: TypedGraphMorphism a b -> [(EdgeId, NodeId, NodeId, EdgeId, NodeId, NodeId, EdgeId)]
  -> TypedGraphMorphism a b
generateNewEdgeInstances =
  foldr (\(a,_,_,b,sb,tb,tp) tgm -> updateEdgeRelation a b (createEdgeOnCodomain b sb tb tp tgm) )



---- Gluing Conditions

-- | Return True if the match @m@ satifies the identification condition for existence of
-- a pushout complement
satisfiesIdentificationCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesIdentificationCondition l m =
  all (==True) (notIdentificatedNodes ++ notIdentificatedEdges)
  where
    notIdentificatedNodes =
      map (notIdentificatedElement l m nodeIdsFromDomain applyNodeId) (nodeIdsFromCodomain m)
    notIdentificatedEdges =
      map (notIdentificatedElement l m edgeIdsFromDomain applyEdgeId) (edgeIdsFromCodomain m)


-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/, two functions /domain/ (to get all elements
-- in the domain of m) and /apply/ (for applying an element in a TypedGraphMorphism),
-- and an element __/e/__ (that can be either a __/Node/__ or an __/Edge/__) it returns true if /e/ is
-- identificated (i.e. __e__ has more than one incident element on itself and at least one of them deletes it)
notIdentificatedElement :: Eq t => TypedGraphMorphism a b -> TypedGraphMorphism a b -> (TypedGraphMorphism a b -> [t])
                         -> (TypedGraphMorphism a b -> t -> Maybe t) -> t -> Bool
notIdentificatedElement l m domain apply e = (length incidentElements <= 1) || not eIsDeleted
  where
    incidentElements = [a | a <- domain m, apply m a == Just e]
    l' = apply (invert l)
    eIsDeleted = Nothing `elem` map l' incidentElements


-- | Given a left-hand-side morphism /l : K -> L/, a match /m : L -> G/,
-- returns @true@ if there are not dangling edges
satisfiesDanglingCondition :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesDanglingCondition l m = Prelude.null incidentEdgesNotDeleted
  where
    lhs = graphDomain m
    instanceGraph = graphCodomain m

    deletedNodes =
      [(n',ctx) |
         (n',ctx) <- nodesInContext instanceGraph,
         any (\n -> applyNodeIdUnsafe m n == nodeId n') (nodeIds lhs),
         isDeleted l m applyNodeId nodeIdsFromDomain (nodeId n')]

    incidentEdgesNotDeleted =
      [edgeId e |
         ((n1,_),e,(n2,_)) <- edgesInContext instanceGraph,
         (n,_) <- deletedNodes,
         nodeId n `elem` [nodeId n1, nodeId n2],
         not (isDeleted l m applyEdgeId edgeIdsFromDomain (edgeId e))]

-- TODO: this function should not be here in this module
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

-- TODO: this function should not be here in this module
isOrphanEdge :: TypedGraphMorphism a b -> EdgeId -> Bool
isOrphanEdge m n = n `elem` orphanTypedEdgeIds m


instance LRNAdhesive (CatM n e) (TypedGraphMorphism n e) where
  ruleMorphism = fixedClass Monomorphisms
  leftHandMorphism = fixedClass Monomorphisms
  matchMorphism = matchMorphismsClass
  hasPushoutComplementOfRN = hasPushoutComplementAlongMono
  calculatePushoutComplementOfRN = calculatePushoutComplementAlongMono


instance InitialPushout (CatM n e) (TypedGraphMorphism n e) where

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
  -- This function receives f, creates the morphism b,
  -- and executes the pushout complement on (1).
  --
  -- The initial pushout operation for typed graphs constructs a graph
  -- with the minimal elements needed in B such that there exists the
  -- pushout (1).
  -- The existence of them ensures that gluing conditions are satisfied.
  -- Two kinds of elements must be created: the nodes that potencially
  -- arises a dangling condition; and the elements that potencially
  -- arises an identification condition.
  calculateInitialPushout f = do
      (d, c) <- calculatePushoutComplementAlongMono f b
      return (b,d,c)
    where
      -- 1. It defines b initially with an empty morphism to A, and then
      -- dangling nodes, collapsed nodes and collapsed edges are added.
      -- d and c are the pushout complement of f and b.
      b = addCollapsedEdges $ addCollapsedNodes $ addDanglingNodes init
        where
          init = emptyMorphismToA
          addDanglingNodes m = addNodes m danglingNodes
          addCollapsedNodes m = addNodes m collapsedNodes
          addCollapsedEdges m = addEdges m collapsedEdges

      -- 2. It just defines the names for the structures
      typeGraph = codomain typedGraphA
      typedGraphA = domain f
      nodeTypesInA = GM.applyNodeIdUnsafe typedGraphA
      edgeTypesInA = GM.applyEdgeIdUnsafe typedGraphA
      graphA = domain typedGraphA
      graphA' = domain (codomain f)
      edgesOfA = edgesFromDomain f
      nodeIdsOfA = nodeIdsFromDomain f

      emptyMorphismToA = buildTypedGraphMorphism emptyTypedGraph typedGraphA emptyMapToA
        where
          emptyTypedGraph = GM.empty empty typeGraph
          emptyMapToA = GM.empty empty graphA


      -- 3. Auxiliary functions

      -- It captures all nodes in A that when mapped to A' has an incident edge.
      danglingNodes = filter checkExistsOrphanIncidentEdge nodeIdsOfA
        where
          checkExistsOrphanIncidentEdge n = any (\(_,e,_) -> isOrphanEdge f (edgeId e)) incEdges
            where
              Just (_,ctx) = lookupNodeInContext (applyNodeIdUnsafe f n) graphA'
              incEdges = incidentEdges ctx

      -- It captures all nodes in A that are equally mapped by f
      collapsedNodes =
        filter
          (\n ->
            any
              (\n' ->
                n/=n' &&
                (applyNodeIdUnsafe f n == applyNodeIdUnsafe f n')
              ) nodeIdsOfA
          ) nodeIdsOfA

      -- It captures all edges in A that are equally mapped by f
      collapsedEdges =
        concatMap
          (\e ->
            [(edgeId e, sourceId e, targetId e) |
              any  (\e' -> edgeId e /= edgeId e' && (applyEdgeIdUnsafe f (edgeId e) == applyEdgeIdUnsafe f (edgeId e'))) edgesOfA]
          ) edgesOfA

      -- It adds a list of nodes in a morphism
      addNodes = foldr (\n -> createNodeOnDomain n (nodeTypesInA n) n)

      -- It adds a list of edges (with source and target nodes) in a morphism
      addEdges =
        foldr
          (\(e,src,tgt) b ->
            (createEdgeOnDomain e src tgt (edgeTypesInA e) e
              (createNodeOnDomain tgt (nodeTypesInA tgt) tgt
                (createNodeOnDomain src (nodeTypesInA src) src b)
              )
            )
          )