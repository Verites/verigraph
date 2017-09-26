{-# LANGUAGE TupleSections #-}
module Data.TypedGraph.MorphismSpec where
  
import           Test.Hspec
import Test.HUnit

import Abstract.Category
import Category.TypedGraph ()
import           Base.Valid
import           Data.Graphs (Node(..), Edge(..))
import qualified Data.Graphs as Graph
import qualified Data.Graphs.Morphism as Graph
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import qualified Data.Relation as Relation


tg = Graph.fromNodesAndEdges [Node 0 Nothing] [Edge 0 0 0 Nothing]
makeGraph ns es = fromNodesAndEdges tg (map (,0) ns) (map (,0) es)


spec :: Spec
spec = do
  describe "fromGraphsAndLists" $ do
    let g1 = makeGraph [Node 0 Nothing, Node 1 Nothing, Node 2 Nothing] [Edge 0 0 1 Nothing, Edge 1 1 0 Nothing, Edge 2 1 1 Nothing, Edge 3 1 2 Nothing]
    let g2 = makeGraph [Node 3 Nothing, Node 4 Nothing, Node 5 Nothing] [Edge 4 3 3 Nothing, Edge 5 3 3 Nothing, Edge 6 3 4 Nothing, Edge 7 4 5 Nothing]
    let f = fromGraphsAndLists g1 g2 [(0,3), (1,3), (2,4)] [(0,4), (1,4), (2,5), (3,6)]

    it "is valid" $
      validate f `shouldBe` IsValid

    it "has the correct domain" $ do
      domain f `shouldBe` g1
      assertEqual "domain of node map" (Relation.domain . Graph.nodeRelation $ mapping f) (nodeIds g1)
      assertEqual "domain of edge map" (Relation.domain . Graph.edgeRelation $ mapping f) (edgeIds g1)
      
    it "has the correct codomain" $ do
      codomain f `shouldBe` g2
      assertEqual "codomain of node map" (Relation.codomain . Graph.nodeRelation $ mapping f) (nodeIds g2)
      assertEqual "codomain of edge map" (Relation.codomain . Graph.edgeRelation $ mapping f) (edgeIds g2)

  describe "makeInclusion" $ do
    let testInclusion dom cod expected = do
          let actual = makeInclusion dom cod
          assertEqual "domain" (domain actual) (domain expected)
          assertEqual "codomain" (codomain actual) (codomain expected)
          assertEqual "mapping - domainGraph" (Graph.domainGraph $ mapping actual) (Graph.domainGraph $ mapping expected)
          assertEqual "mapping - codomainGraph" (Graph.codomainGraph $ mapping actual) (Graph.codomainGraph $ mapping expected)
          assertEqual "mapping - nodeRelation" (Graph.nodeRelation $ mapping actual) (Graph.nodeRelation $ mapping expected)
          assertEqual "mapping - edgeRelation" (Graph.edgeRelation $ mapping actual) (Graph.edgeRelation $ mapping expected)
          assertEqual "mapping" (mapping actual) (mapping expected)
          actual `shouldBe` expected

    it "produces the identity when domain = codomain" $
      let g = makeGraph [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 Nothing, Edge 1 0 1 Nothing, Edge 2 1 0 Nothing]
      in testInclusion g g $ fromGraphsAndLists g g [(0,0), (1,1)] [(0,0), (1,1), (2,2)]

    it "produces an inclusion when nodes are absent from domain" $
      let 
        g  = makeGraph [Node 0 Nothing, Node 1 Nothing, Node 2 Nothing] [Edge 0 0 0 Nothing, Edge 1 0 1 Nothing, Edge 2 1 0 Nothing]
        g' = makeGraph [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 Nothing, Edge 1 0 1 Nothing, Edge 2 1 0 Nothing]
      in testInclusion g' g $ fromGraphsAndLists g' g [(0,0), (1,1)] [(0,0), (1,1), (2,2)]
      
    it "produces an inclusion when edges are absent from domain" $
      let
        g  = makeGraph [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 Nothing, Edge 1 0 1 Nothing, Edge 2 1 0 Nothing]
        g' = makeGraph [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 Nothing, Edge 2 1 0 Nothing]
      in testInclusion g' g $ fromGraphsAndLists g' g [(0,0), (1,1)] [(0,0), (2,2)]