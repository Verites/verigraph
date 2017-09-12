module Data.TypedGraph.Morphism.FindMorphismSpec.FindCospanCommuterTest (findCospanCommuterTest) where

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Category.TypedGraph                ()
import           Data.Graphs
import qualified Data.Graphs.Morphism               as GM
import qualified Data.TypedGraph.Morphism           as TGM
import           Test.Hspec

type TGM a b = TGM.TypedGraphMorphism a b
type GM a b = GM.GraphMorphism (Maybe a) (Maybe b)
type G a b = Graph (Maybe a) (Maybe b)

findCospanCommuterTest :: Spec
findCospanCommuterTest =
  it "Should return the same value as the exhaustive method" $ do

  -- | Tests with only nodes
  genericTest tgm1 tgm1

  genericTest tgm3 tgm2
  genericTest tgm2 tgm3

  genericTest tgm3 tgm1
  genericTest tgm1 tgm3

  -- | Not a valid Cospan:  @B -> A@ and @A' <- C@
  genericTest tgm4 tgm1
  genericTest tgm1 tgm4

  genericTest tgm5 tgm1
  genericTest tgm1 tgm5

  -- | Tests with edges
  genericTest tgm6 tgm6

  genericTest tgm7 tgm6
  genericTest tgm6 tgm7

  genericTest tgm9 tgm8
  genericTest tgm8 tgm9

  -- | Tests with loops
  genericTest tgm10 tgm11
  genericTest tgm11 tgm10


-- | TypedGraphMorphism instances. Digraphs with only nodes

tgm1 :: TGM a b
tgm1 = TGM.buildTypedGraphMorphism g2 g2 $ GM.buildGraphMorphism g2' g2' [(1,1),(2,2)] []

tgm2 :: TGM a b
tgm2 = TGM.buildTypedGraphMorphism g2 g1 $ GM.buildGraphMorphism g2' g1' [(1,1),(2,1)] []

tgm3 :: TGM a b
tgm3 = TGM.buildTypedGraphMorphism g1 g1 $ GM.buildGraphMorphism g1' g1' [(1,1)] []

tgm4 :: TGM a b
tgm4 = TGM.buildTypedGraphMorphism g1 g2 $ GM.buildGraphMorphism g1' g2' [(1,1)] []

tgm5 :: TGM a b
tgm5 = TGM.buildTypedGraphMorphism g3 g2 $ GM.buildGraphMorphism g3' g2' [(1,1),(2,2),(3,2)] []


-- | TypedGraphMorphism instances. Digraphs with only nodes

tgm6 :: TGM a b
tgm6 = TGM.buildTypedGraphMorphism g4 g4 $ GM.buildGraphMorphism g4' g4' [(1,1),(2,2)] [(1,1)]

tgm7 :: TGM a b
tgm7 = TGM.buildTypedGraphMorphism g5 g4 $ GM.buildGraphMorphism g5' g4' [(1,1),(2,2)][(1,1),(2,1)]

tgm8 :: TGM a b
tgm8 = TGM.buildTypedGraphMorphism g5 g5 $ GM.buildGraphMorphism g5' g5' [(1,1),(2,2)][(1,1),(2,2)]

tgm9 :: TGM a b
tgm9 = TGM.buildTypedGraphMorphism g6 g5 $ GM.buildGraphMorphism g6' g5' [(1,1),(2,2)][(1,1),(2,2),(3,1)]


-- | TypedGraphMorphism instances. Digraphs with loops

tgm10 :: TGM a b
tgm10 = TGM.buildTypedGraphMorphism g7 g7 $ GM.buildGraphMorphism g7' g7' [(1,1)] [(1,1)]

tgm11 :: TGM a b
tgm11 = TGM.buildTypedGraphMorphism g8 g7 $ GM.buildGraphMorphism g8' g7' [(1,1),(2,1)][(1,1),(2,1)]


-- | Graphs instances for tests

typegraph :: Graph (Maybe a) (Maybe b)
typegraph = build [1,2] [(1,1,2)]


-- | Digraphs with only nodes

-- |digraph G1 {
--   1 [shape = circle];
-- }
g1 :: GM a b
g1 = GM.buildGraphMorphism g1' typegraph [(1,1)] []

g1' :: G a b
g1' = build [1] []

-- |digraph G2 {
--   1 [shape = circle];
--   2 [shape = circle];
-- }
g2 :: GM a b
g2 = GM.buildGraphMorphism g2' typegraph [(1,1),(2,1)] []

g2' :: G a b
g2' = build [1,2] []

-- |digraph G3 {
--   1 [shape = circle];
--   2 [shape = box];
--   3 [shape = circle];
-- }
g3 :: GM a b
g3 = GM.buildGraphMorphism g3' typegraph [(1,1),(2,1),(3,1)] []

g3' :: G a b
g3' = build [1,2,3] []


-- | Digraphs with edges

-- |digraph G4 {
--   1 [shape = circle];
--   2 [shape = circle];
--
--   1 -> 2;
--
-- }
g4 :: GM a b
g4 = GM.buildGraphMorphism g4' typegraph [(1,1),(2,1)] [(1,1)]

g4' :: G a b
g4' = build [1,2] [(1,1,2)]

-- |digraph G5 {
--   1 [shape = circle];
--   2 [shape = circle];
--
--   1 -> 2;
--   1 -> 2;
--
-- }
g5 :: GM a b
g5 = GM.buildGraphMorphism g5' typegraph [(1,1),(2,1)] [(1,1),(2,1)]

g5' :: G a b
g5' = build [1,2] [(1,1,2),(2,1,2)]

-- |digraph G6 {
--   1 [shape = circle];
--   2 [shape = circle];
--
--   1 -> 2;
--   1 -> 2;
--   1 -> 2;
--
-- }
g6 :: GM a b
g6 = GM.buildGraphMorphism g6' typegraph [(1,1),(2,1)] [(1,1),(2,1),(3,1)]

g6' :: G a b
g6' = build [1,2] [(1,1,2),(2,1,2),(3,1,2)]


-- | Digraphs with loops for validity tests

-- | digraph G7 {
--    1 [shape = circle];
--
--    1 -> 1;
--
--   }
g7 :: GM a b
g7 = GM.buildGraphMorphism g7' typegraph [(1,1)] [(1,1)]

g7' :: G a b
g7' = build [1] [(1,1,1)]

-- | digraph G8 {
--    1 [shape = circle];
--    2 [shape = circle];
--
--    1 -> 1;
--    2 -> 2;
--
--   }
g8 :: GM a b
g8 = GM.buildGraphMorphism g8' typegraph [(1,1),(2,1)] [(1,1),(2,1)]

g8' :: G a b
g8' = build [1,2] [(1,1,1),(2,2,2)]


-- | Auxiliary functions to tests

genericTest :: TGM a b -> TGM a b -> Expectation
genericTest morphismOne morphismTwo =
  do
    genericCompare anyMorphism morphismOne morphismTwo
    genericCompare monic morphismOne morphismTwo
    genericCompare epic morphismOne morphismTwo
    genericCompare iso morphismOne morphismTwo

genericCompare :: MorphismClass (TGM a b) -> TGM a b -> TGM a b -> Expectation
genericCompare conf morphismOne morphismTwo =
  findCospanCommuters conf morphismOne morphismTwo
  `shouldBe` genericCommuter conf morphismOne morphismTwo

genericCommuter :: MorphismClass (TGM a b) -> TGM a b -> TGM a b -> [TGM a b]
genericCommuter conf morphismOne morphismTwo = 
  filter commutes $ findMorphisms conf (domain morphismOne) (domain morphismTwo)
  where commutes x = morphismOne == morphismTwo <&> x
