import           Abstract.Morphism
import qualified Abstract.Relation   as R
import           Data.List
import qualified Data.Map            as M
import           Graph.Graph
import qualified Graph.GraphMorphism as GM
import           Test.HUnit
import qualified TypedGraph.Morphism as TGM
import           Utils

-- Type shorthands
type TGM a b = TGM.TypedGraphMorphism a b
type GM a b = GM.GraphMorphism a b
type CanonicalMorphism = ([(NodeId, NodeId)], [(EdgeId, EdgeId)])

main :: IO()
main = do
  runTests ("Preliminary tests with number of morphisms" ~: testsLen)
  runTests ("Effective tests with results of morphisms" ~: tests)

--Generic test with number of results
genericLenTest :: GM a b -> GM a b -> (Int,Int,Int,Int)
genericLenTest src tgt = (length $ mono src tgt,
                          length $ surj src tgt,
                          length $ isom src tgt,
                          length $ allm src tgt)
  where
    mono :: GM a b -> GM a b -> [TGM a b ]
    mono src tgt = findMonomorphisms src tgt

    surj :: GM a b -> GM a b -> [TGM a b ]
    surj src tgt = findMorphisms Epimorphism src tgt

    isom :: GM a b -> GM a b -> [TGM a b ]
    isom src tgt = findIsomorphisms src tgt

    allm :: GM a b -> GM a b -> [TGM a b ]
    allm src tgt = findAllMorphisms src tgt


--Generic test with number of results
genericTest :: GM a b -> GM a b -> ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
genericTest src tgt = (sort . map toCanonical $ mono src tgt,
                       sort . map toCanonical $ surj src tgt,
                       sort . map toCanonical $ isom src tgt,
                       sort . map toCanonical $ allm src tgt)
  where
    mono :: GM a b -> GM a b -> [TGM a b]
    mono src tgt = findMonomorphisms src tgt

    surj :: GM a b -> GM a b -> [TGM a b]
    surj src tgt = findMorphisms Epimorphism src tgt

    isom :: GM a b -> GM a b -> [TGM a b]
    isom src tgt = findIsomorphisms src tgt

    allm :: GM a b -> GM a b -> [TGM a b]
    allm src tgt = findAllMorphisms src tgt

    toCanonical :: TGM a b -> CanonicalMorphism
    toCanonical tgm =
      let gm = TGM.mapping tgm
          relationAsFunction = map $ \(x, [y]) -> (x, y)
          nodeMapping = sort . relationAsFunction . M.toList . R.mapping $ GM.nodeRelation gm
          edgeMapping = sort . relationAsFunction . M.toList . R.mapping $ GM.edgeRelation gm
      in (nodeMapping, edgeMapping)


--Testing Graphs
typegraph :: Graph a b
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


--Test cases with results of morphisms
test1 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test1  = genericTest g1 g2

test1' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test1' = genericTest g2 g1

test2 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test2  = genericTest g3 g4

test2' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test2' = genericTest g4 g3

test3 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test3  = genericTest g5 g3

test3' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test3' = genericTest g3 g5

test4 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test4  = genericTest g5 g5

test5 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test5  = genericTest g9 g4

test5' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test5' = genericTest g4 g9

test6 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test6  = genericTest g2 g12

test6' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test6' = genericTest g12 g2

test7 :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test7  = genericTest g13 g4

test7' :: ([CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism], [CanonicalMorphism])
test7' = genericTest g4 g13


--Test cases with number of morphisms
testLen1 :: (Int,Int, Int, Int)
testLen1  = genericLenTest g1 g2

testLen1' :: (Int,Int, Int, Int)
testLen1' = genericLenTest g2 g1

testLen2 :: (Int,Int, Int, Int)
testLen2  = genericLenTest g3 g4

testLen2' :: (Int,Int, Int, Int)
testLen2' = genericLenTest g4 g3

testLen3 :: (Int,Int, Int, Int)
testLen3  = genericLenTest g5 g3

testLen3' :: (Int,Int, Int, Int)
testLen3' = genericLenTest g3 g5

testLen4 :: (Int,Int, Int, Int)
testLen4  = genericLenTest g5 g5

testLen5 :: (Int,Int, Int, Int)
testLen5  = genericLenTest g9 g4

testLen5' :: (Int,Int, Int, Int)
testLen5' = genericLenTest g4 g9

testLen6 :: (Int,Int, Int, Int)
testLen6  = genericLenTest g2 g12

testLen6' :: (Int,Int, Int, Int)
testLen6' = genericLenTest g12 g2

testLen7 :: (Int,Int, Int, Int)
testLen7  = genericLenTest g13 g4

testLen7' :: (Int,Int, Int, Int)
testLen7' = genericLenTest g4 g13


--Verifying morphism results
tests :: Test
tests = test [ "Test 1 "  ~: test1  ~=? ( []
                                        , [([(1,1),(2,2)],[(3,3),(4,3)])]
                                        , []
                                        , [([(1,1),(2,2)],[(3,3),(4,3)])] )

             , "Test 1' " ~: test1' ~=? ( [([(1,1),(2,2)],[(3,3)])
                                          ,([(1,1),(2,2)],[(3,4)])]
                                        , []
                                        , []
                                        , [([(1,1),(2,2)],[(3,3)])
                                          ,([(1,1),(2,2)],[(3,4)])] )

             , "Test 2 "  ~: test2  ~=? ( []
                                        , [ ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5)])
                                          , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4)])]
                                        , []
                                        , [ ([(1,1),(2,3),(3,1),(4,3)],[(5,4),(6,4)])
                                          , ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5)])
                                          , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4)])
                                          , ([(1,2),(2,3),(3,2),(4,3)],[(5,5),(6,5)])] )

             , "Test 2' " ~: test2'  ~=? ( []
                                         , []
                                         , []
                                         , [ ([(1,1),(2,1),(3,2)],[(4,5),(5,5)])
                                           , ([(1,3),(2,3),(3,4)],[(4,6),(5,6)])] )

             , "Test 3 "  ~: test3  ~=? ( []
                                        , []
                                        , []
                                        , [ ([(1,1),(2,2),(3,2)],[(4,5),(5,5)])
                                          , ([(1,3),(2,4),(3,4)],[(4,6),(5,6)])] )

             , "Test 3' " ~: test3' ~=? ( []
                                        , [ ([(1,1),(2,2),(3,1),(4,3)],[(5,4),(6,5)])
                                          , ([(1,1),(2,3),(3,1),(4,2)],[(5,5),(6,4)])]
                                        , []
                                        , [ ([(1,1),(2,2),(3,1),(4,2)],[(5,4),(6,4)])
                                          , ([(1,1),(2,2),(3,1),(4,3)],[(5,4),(6,5)])
                                          , ([(1,1),(2,3),(3,1),(4,2)],[(5,5),(6,4)])
                                          , ([(1,1),(2,3),(3,1),(4,3)],[(5,5),(6,5)])] )

             , "Test 4 "  ~: test4  ~=? ( [ ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                                          , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])]
                                        , [ ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                                          , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])]
                                        , [ ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                                          , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])]
                                        , [ ([(1,1),(2,2),(3,2)],[(4,4),(5,4)])
                                          , ([(1,1),(2,2),(3,3)],[(4,4),(5,5)])
                                          , ([(1,1),(2,3),(3,2)],[(4,5),(5,4)])
                                          , ([(1,1),(2,3),(3,3)],[(4,5),(5,5)])] )

             , "Test 5 "  ~: test5  ~=? ( []
                                        , [ ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5),(7,5),(8,4)])
                                          , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4),(7,4),(8,5)])]
                                        , []
                                        , [ ([(1,1),(2,3),(3,1),(4,3)],[(5,4),(6,4),(7,4),(8,4)])
                                          , ([(1,1),(2,3),(3,2),(4,3)],[(5,4),(6,5),(7,5),(8,4)])
                                          , ([(1,2),(2,3),(3,1),(4,3)],[(5,5),(6,4),(7,4),(8,5)])
                                          , ([(1,2),(2,3),(3,2),(4,3)],[(5,5),(6,5),(7,5),(8,5)])] )

             , "Test 5' " ~: test5' ~=? ( [ ([(1,1),(2,3),(3,2)],[(4,5),(5,7)])
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

             , "Test 6 "  ~: test6  ~=? ( []
                                        , [ ([(1,1),(2,1)],[(3,2)])]
                                        , []
                                        , [ ([(1,1),(2,1)],[(3,2)])] )

             , "Test 6' " ~: test6' ~=? ( []
                                        , []
                                        , []
                                        , [] )

             , "Test 7 "  ~: test7  ~=? ( [ ([(1,1),(2,3)],[(3,4)]),([(1,2),(2,3)],[(3,5)])]
                                        , []
                                        , []
                                        , [ ([(1,1),(2,3)],[(3,4)])
                                          , ([(1,2),(2,3)],[(3,5)])] )

             , "Test 7' " ~: test7' ~=? ( []
                                        , [([(1,1),(2,1),(3,2)],[(4,3),(5,3)])]
                                        , []
                                        , [([(1,1),(2,1),(3,2)],[(4,3),(5,3)])] )
             ]

  --Verify number of morphisms results
testsLen :: Test
testsLen = test [ "TestLen 1 "  ~: testLen1  ~=? (0,1,0,1)
                , "TestLen 1' " ~: testLen1' ~=? (2,0,0,2)

                , "TestLen 2 "  ~: testLen2  ~=? (0,2,0,4)
                , "TestLen 2' " ~: testLen2' ~=? (0,0,0,2)

                , "TestLen 3 "  ~: testLen3  ~=? (0,0,0,2)
                , "TestLen 3' " ~: testLen3' ~=? (0,2,0,4)

                , "TestLen 4 "  ~: testLen4  ~=? (2,2,2,4)

                , "TestLen 5 "  ~: testLen5  ~=? (0,2,0,4)
                , "TestLen 5' " ~: testLen5' ~=? (4,0,0,8)

                , "TestLen 6 "  ~: testLen6  ~=? (0,1,0,1)
                , "TestLen 6' " ~: testLen6' ~=? (0,0,0,0)

                , "TestLen 7 "  ~: testLen7  ~=? (2,0,0,2)
                , "TestLen 7' " ~: testLen7' ~=? (0,1,0,1) ]
