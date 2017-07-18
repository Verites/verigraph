module Category.TypedGraph.FinalPullbackComplement where

import           Abstract.Category.FinalPullbackComplement
import           Abstract.Category.FinitaryCategory
import           Category.TypedGraph.Cocomplete     ()
import           Data.Graphs                        as G
import qualified Data.Graphs.Morphism               as GM
import           Data.TypedGraph.Morphism

instance FinalPullbackComplement (TypedGraphMorphism a b) where

  -- @
  --       l
  --    K──────▶L
  --    │       V
  --  k │  (1)  │ m
  --    ▼       ▼
  --    D──────▶A
  --       l'
  -- @
  --
  -- This function receives m and l, it creates (k,l') as the
  -- the final pullback complement on (1).
  --
  -- __morphism m must be injective__
  --
  -- The algorithm follows Construction 6 of Sesqui-pushout rewriting.
  -- Available on:
  -- http://www.ti.inf.uni-due.de/publications/koenig/icgt06b.pdf
  --
  -- It is a naive implementation focused on correction and not performance.
  -- Performance may be reasonable for epi pairs rewrite, but poor when large contexts.
  --
  -- The resulting graph D contains a copy of K, a copy of the largest
  -- subgraph of A which is not in the image of m, and a suitable number
  -- of copies of each edge of A incident to a node in m(l(K)):
  -- this has the effect of "cloning" part of A.
  --
  -- This function is divided in four steps,
  -- first two for nodes and the lasts for edges.
  calculateFinalPullbackComplement m l = step4
    where
      typedGraphK = domain l
      typedGraphA = codomain m
      graphK = domain typedGraphK
      graphA = domain typedGraphA
      edgeTypeInK = GM.applyEdgeIdUnsafe typedGraphK
      edgeTypeInA = GM.applyEdgeIdUnsafe typedGraphA
      nodeTypeInK = GM.applyNodeIdUnsafe typedGraphK
      nodeTypeInA = GM.applyNodeIdUnsafe typedGraphA
      typeGraph = codomain typedGraphK

      -- Inits (k:K->D, l':D->A) with D as empty.
      initD = GM.empty empty typeGraph
      initK = buildTypedGraphMorphism typedGraphK initD (GM.empty graphK empty)
      initL' = buildTypedGraphMorphism initD typedGraphA (GM.empty empty graphA)

      -- Step1 adds in D a copy of the nodes of K.
      step1 = foldr updateNodesFromK (initK,initL') nodesAddFromK
      nodesAddFromK = zip (nodeIdsFromDomain l) ([0..]::[Int])
      updateNodesFromK (n,newId) (k,l') = (updatedK2,updatedL')
        where
          newNode = NodeId newId
          typeN = nodeTypeInK n
          appliedL = applyNodeIdUnsafe l n
          appliedA = applyNodeIdUnsafe m appliedL
          updatedK = createNodeOnCodomain newNode typeN k
          updatedK2 = untypedUpdateNodeRelation n newNode updatedK
          updatedL' = createNodeOnDomain newNode typeN appliedA l'

      -- Step2 adds in D the nodes out of the image of m.
      step2 = foldr updateNodesFromA step1 nodesAddFromMatch
      nodesAddFromMatch = zip (orphanTypedNodeIds m) ([(length nodesAddFromK)..]::[Int])
      updateNodesFromA (n,newId) (k,l') = (updatedK,updatedL')
        where
          newNode = NodeId newId
          typeN = nodeTypeInA n
          updatedK = createNodeOnCodomain newNode typeN k
          updatedL' = createNodeOnDomain newNode typeN n l'

      -- Step3 adds in D a copy of the edges of K.
      step3@(_,edgesL') = foldr updateEdgesFromK step2 edgesAddFromK
      edgesAddFromK = zip (edgesFromDomain l) ([0..]::[Int])
      updateEdgesFromK (e,newId) (k,l') = (updatedK2,updatedL')
        where
          newEdge = EdgeId newId
          appliedL = applyEdgeIdUnsafe l (edgeId e)
          appliedA = applyEdgeIdUnsafe m appliedL
          typeE = edgeTypeInK (edgeId e)
          src = applyNodeIdUnsafe k (sourceId e)
          tgt = applyNodeIdUnsafe k (targetId e)
          updatedK = createEdgeOnCodomain newEdge src tgt typeE k
          updatedK2 = updateEdgeRelation (edgeId e) newEdge updatedK
          updatedL' = createEdgeOnDomain newEdge src tgt typeE appliedA l'

      -- Step4 adds in D a replication of edges out of the image of m,
      -- where source and target nodes may have been cloned in D.
      step4 = foldr updateEdgesFromA step3 edgesAddFromMatch
      edgesAddFromMatch = zip edgesFromA ([(length edgesAddFromK)..]::[Int])
        where
          edgesFromA = [(edgeId e, u, v) |
                         e <- orphanTypedEdges m,
                         u <- nodeIdsFromDomain edgesL',
                         v <- nodeIdsFromDomain edgesL',
                         sourceId e == applyNodeIdUnsafe edgesL' u,
                         targetId e == applyNodeIdUnsafe edgesL' v]
      updateEdgesFromA ((e,u,v),newId) (k,l') = (updatedK,updatedL')
        where
          newEdge = EdgeId newId
          typeE = edgeTypeInA e
          updatedK = createEdgeOnCodomain newEdge u v typeE k
          updatedL' = createEdgeOnDomain newEdge u v typeE e l'

  hasFinalPullbackComplement (Monomorphism, _) _ = True
  hasFinalPullbackComplement _ _ =
    error "Final pullback complement is not implemented for non monomorphic matches"
