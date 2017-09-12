module Data.TypedGraph.Morphism.FindMorphismSpec.FindMorphismsTest (findMorphismsTest) where

import           Abstract.Category.FindMorphism
import           Category.TypedGraph                ()
import           Data.Graphs
import qualified Data.Graphs.Morphism               as GM
import           Data.List
import qualified Data.Map                           as M
import qualified Data.Relation                      as R
import qualified Data.TypedGraph.Morphism           as TGM
import           Test.Hspec

type TGM a b = TGM.TypedGraphMorphism a b
type GM a b = GM.GraphMorphism (Maybe a) (Maybe b)
type CanonicalMorphism = ([(NodeId, NodeId)], [(EdgeId, EdgeId)])

findMorphismsTest :: Spec
findMorphismsTest =
  it "Should return the expected values" $
  do
    test1  `shouldBe` ( []
                       , [([(1,1),(2,2)],[(3,3),(4,3)])]
                       , []
                       , [([(1,1),(2,2)],[(3,3),(4,3)])] )

    test1' `shouldBe` ( [([(1,1),(2,2)],[(3,3)])
                        ,([(1,1),(2,2)],[(3,4)])]
                      , []
                      , []
                      , [([(1,1),(2,2)],[(3,3)])
                        ,([(1,1),(2,2)],[(3,4)])] )

    test2 `shouldBe` ( []
                     , [ ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5)])
                       , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4)])]
                     , []
                     , [ ([(1,1),(2,3),(3,1),(4,3)],[(5,4),(6,4)])
                       , ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5)])
                       , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4)])
                       , ([(1,2),(2,3),(3,2),(4,3)],[(5,5),(6,5)])] )

    test2'  `shouldBe` ( []
                       , []
                       , []
                       , [ ([(1,1),(2,1),(3,2)],[(4,5),(5,5)])
                         , ([(1,3),(2,3),(3,4)],[(4,6),(5,6)])] )

    test3  `shouldBe` ( []
                      , []
                      , []
                      , [ ([(1,1),(2,2),(3,2)],[(4,5),(5,5)])
                        , ([(1,3),(2,4),(3,4)],[(4,6),(5,6)])] )

    test3' `shouldBe` ( []
                      , [ ([(1,1),(2,2),(3,1),(4,3)],[(5,4),(6,5)])
                        , ([(1,1),(2,3),(3,1),(4,2)],[(5,5),(6,4)])]
                      , []
                      , [ ([(1,1),(2,2),(3,1),(4,2)],[(5,4),(6,4)])
                        , ([(1,1),(2,2),(3,1),(4,3)],[(5,4),(6,5)])
                        , ([(1,1),(2,3),(3,1),(4,2)],[(5,5),(6,4)])
                        , ([(1,1),(2,3),(3,1),(4,3)],[(5,5),(6,5)])] )

    test4  `shouldBe` ( [ ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                        , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])]
                      , [ ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                        , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])]
                      , [ ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                        , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])]
                      , [ ([(1,1),(2,2),(3,2)],[(4,4),(5,4)])
                        , ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                        , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])
                        , ([(1,1),(2,3),(3,3)],[(4,5),(5,5)])] )

    test5  `shouldBe` ( []
                      , [ ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5),(7,5),(8,4)])
                        , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4),(7,4),(8,5)])]
                      , []
                      , [ ([(1,1),(2,3),(3,1),(4,3)],[(5,4),(6,4),(7,4),(8,4)])
                        , ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5),(7,5),(8,4)])
                        , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4),(7,4),(8,5)])
                        , ([(1,2),(2,3),(3,2),(4,3)],[(5,5),(6,5),(7,5),(8,5)])] )

    test5' `shouldBe` ( [ ([(1,1),(2,3),(3,2)],[(4,5),(5,7)])
                        , ([(1,1),(2,3),(3,4)],[(4,8),(5,6)])
                        , ([(1,3),(2,1),(3,2)],[(4,7),(5,5)])
                        , ([(1,3),(2,1),(3,4)],[(4,6),(5,8)])]
                      , []
                      , []
                      , [ ([(1,1),(2,1),(3,2)],[(4,5),(5,5)])
                        , ([(1,1),(2,1),(3,4)],[(4,8),(5,8)])
                        , ([(1,1),(2,3),(3,2)],[(4,5),(5,7)])
                        , ([(1,1),(2,3),(3,4)],[(4,8),(5,6)])
                        , ([(1,3),(2,1),(3,2)],[(4,7),(5,5)])
                        , ([(1,3),(2,1),(3,4)],[(4,6),(5,8)])
                        , ([(1,3),(2,3),(3,2)],[(4,7),(5,7)])
                        , ([(1,3),(2,3),(3,4)],[(4,6),(5,6)])] )

    test6  `shouldBe` ( []
                      , [ ([(1,1),(2,1)],[(3,2)])]
                      , []
                      , [ ([(1,1),(2,1)],[(3,2)])] )

    test6' `shouldBe` ( []
                      , []
                      , []
                      , [] )

    test7  `shouldBe` ( [ ([(1,1),(2,3)],[(3,4)]),([(1,2),(2,3)],[(3,5)])]
                      , []
                      , []
                      , [ ([(1,1),(2,3)],[(3,4)])
                        , ([(1,2),(2,3)],[(3,5)])] )

    test7' `shouldBe` ( []
                      , [([(1,1),(2,1),(3,2)],[(4,3),(5,3)])]
                      , []
                      , [([(1,1),(2,1),(3,2)],[(4,3),(5,3)])] )

-- | Generic function to combine all monomorphisms, epimorphisms, isomorphisms and generic morphisms for two given instances.
genericApply :: GM a b -> GM a b -> ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
genericApply src tgt = (sort . map toCanonical $ findMonomorphisms src tgt,
                       sort . map toCanonical $ findEpimorphisms src tgt,
                       sort . map toCanonical $ findIsomorphisms src tgt,
                       sort . map toCanonical $ findAllMorphisms src tgt)
  where
    toCanonical :: TGM a b -> CanonicalMorphism
    toCanonical tgm =
      let gm = TGM.mapping tgm
          relationAsFunction = map $ \(x, [y]) -> (x, y)
          nodeMapping = sort . relationAsFunction . M.toList . R.mapping $ GM.nodeRelation gm
          edgeMapping = sort . relationAsFunction . M.toList . R.mapping $ GM.edgeRelation gm
      in (nodeMapping, edgeMapping)


-- | Graphs instances for build tests

typegraph :: Graph (Maybe a) (Maybe b)
typegraph = build [1,2] [(3,1,1),(4,2,2),(5,1,2),(6,2,1)]


-- |digraph G1 {
-- |  1 [shape = circle];
-- |  2 [shape = circle];
-- |
-- |  1 -> 2;
-- |  1 -> 2;
-- |}
g1 :: GM a b
g1 = GM.buildGraphMorphism g1' typegraph [(1,1),(2,1)] [(3,3),(4,3)]
  where
    g1' = build [1,2] [(3,1,2),(4,1,2)]

-- |digraph G2 {
-- |  1 [shape = circle];
-- |  2 [shape = circle];
-- |
-- |  1 -> 2;
-- |}
g2 :: GM a b
g2 = GM.buildGraphMorphism g2' typegraph [(1,1),(2,1)] [(3,3)]
  where
    g2' = build [1,2] [(3,1,2)]

-- |digraph G3 {
-- |  1 [shape = circle];
-- |  2 [shape = box];
-- |  3 [shape = circle];
-- |  4 [shape = box];
-- |
-- |  1 -> 2;
-- |  3 -> 4;
-- |}
g3 :: GM a b
g3 = GM.buildGraphMorphism g3' typegraph [(1,1),(2,2),(3,1),(4,2)] [(5,5), (6,5)]
  where
    g3' = build [1,2,3,4] [(5,1,2),(6,3,4)]

-- |digraph G4 {
-- |  1 [shape = circle];
-- |  2 [shape = circle];
-- |  3 [shape = box];
-- |
-- |  1 -> 3;
-- |  2 -> 3;
-- |}
g4 :: GM a b
g4 = GM.buildGraphMorphism g4' typegraph [(1,1),(2,1),(3,2)] [(4,5),(5,5)]
  where
    g4' = build [1,2,3] [(4,1,3),(5,2,3)]

-- |digraph G5 {
-- |  1 [shape = circle];
-- |  2 [shape = box];
-- |  3 [shape = box];
-- |
-- |  1 -> 2;
-- |  1 -> 3;
-- |}
g5 :: GM a b
g5 = GM.buildGraphMorphism g5' typegraph [(1,1),(2,2),(3,2)] [(4,5),(5,5)]
  where
    g5' = build [1,2,3] [(4,1,2),(5,1,3)]

-- |digraph G9 {
-- |  1 [shape = circle];
-- |  2 [shape = box];
-- |  3 [shape = circle];
-- |  4 [shape = box];
-- |
-- |  1 -> 2;
-- |  1 -> 4;
-- |  3 -> 2;
-- |  3 -> 4;
-- |}
g9 :: GM a b
g9 = GM.buildGraphMorphism g9' typegraph [(1,1),(2,2),(3,1),(4,2)] [(5,5),(6,5),(7,5),(8,5)]
  where
    g9' = build [1,2,3,4] [(5,1,2),(6,3,4),(7,3,2),(8,1,4)]

-- |digraph G12 {
-- |  1 [shape = circle];
-- |
-- |  1 -> 1;
-- |}
g12 :: GM a b
g12 = GM.buildGraphMorphism g12' typegraph [(1,1)] [(2,3)]
  where
    g12' = build [1] [(2,1,1)]

-- |digraph G13 {
-- |  1 [shape = circle];
-- |  2 [shape = box];
-- |
-- |  1 -> 2;
-- |}
g13 :: GM a b
g13 = GM.buildGraphMorphism g13' typegraph [(1,1),(2,2)] [(3,5)]
  where
    g13' = build [1,2] [(3,1,2)]

-- | Tests instances
test1 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test1  = genericApply g1 g2

test1' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test1' = genericApply g2 g1

test2 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test2  = genericApply g3 g4

test2' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test2' = genericApply g4 g3

test3 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test3  = genericApply g5 g3

test3' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test3' = genericApply g3 g5

test4 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test4  = genericApply g5 g5

test5 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test5  = genericApply g9 g4

test5' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test5' = genericApply g4 g9

test6 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test6  = genericApply g2 g12

test6' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test6' = genericApply g12 g2

test7 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test7  = genericApply g13 g4

test7' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test7' = genericApply g4 g13
