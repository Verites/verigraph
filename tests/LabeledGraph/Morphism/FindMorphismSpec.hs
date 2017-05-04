{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LabeledGraph.Morphism.FindMorphismSpec where


import           Abstract.Morphism       as Morphism
import           Abstract.Valid
import           Abstract.Variable
import           LabeledGraph            as Graph
import           LabeledGraph.Morphism   as Morphism
import           LabeledGraph.QuickCheck


import           Control.Monad
import           Data.Foldable
import           Data.Text.Arbitrary     ()
import           Math.Combinat.Numbers   (stirling2nd)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck         as QuickCheck



maxGraphSize :: Int
maxGraphSize = 5


withSmallerGraphs :: SpecWith a -> SpecWith a
withSmallerGraphs =
  modifyMaxSize (`div` 4)


spec :: Spec
spec = withSmallerGraphs $ do

  describe "findMorphisms" $ do

    prop "always produces valid morphisms" $ \domain codomain ->
      forAllMorphismsBetween GenericMorphism domain codomain $ \m ->
        isValid (m :: LabeledMorphism)


    context "from any empty graph" $ do

      it "always produces a single morphism" $
        property $ \codomain ->
          length (findMorphisms GenericMorphism Graph.empty codomain :: [LabeledMorphism]) == 1

      it "always produces a single monomorphism" $
        property $ \codomain ->
          length (findMorphisms Monomorphism Graph.empty codomain :: [LabeledMorphism]) == 1

      it "always produces no epimorphism (unless the codomain is empty)" $
        property $ \codomain ->
          length (findMorphisms Epimorphism Graph.empty codomain :: [LabeledMorphism])
            == if Graph.null codomain then 1 else 0

      it "always produces no isomorphism (unless the codomain is empty)" $
        property $ \codomain ->
          length (findMorphisms Isomorphism Graph.empty codomain :: [LabeledMorphism])
            == if Graph.null codomain then 1 else 0


    context "between graphs with no edges or labels" $ do
      let makeGraph v = fromNodesAndEdges [ Node n Nothing | n <- [0 .. toEnum (v - 1)] ] []

      it "always produces v2^v1 morphisms" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..maxGraphSize] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          (v2 ^ v1)
          (length (findMorphisms GenericMorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces v2!/(v2-v1)! monomorphisms when v2 >= v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [v1..maxGraphSize] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          (foldl' (*) 1 $ take v1 [v2, v2-1 .. 1])
          (length (findMorphisms Monomorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces no monomorphisms when v2 < v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..v1-1] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          0
          (length (findMorphisms Monomorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces S(v1, v2) * v2! epimorphisms when v2 <= v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..v1] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          (fromInteger (stirling2nd v1 v2) * foldl' (*) 1 [1..v2])
          (length (findMorphisms Epimorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces no epimorphisms when v2 > v1" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [v1+1..maxGraphSize] $ \v2 ->
        assertEqual ("with " ++ show (v1, v2))
          0
          (length (findMorphisms Epimorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces no isomorphism when v1 /= v2" $
        forM_ [1..maxGraphSize] $ \v1 ->
        forM_ [0..maxGraphSize] $ \v2 ->
        when (v1 /= v2) $
        assertEqual ("with " ++ show (v1, v2))
          0
          (length (findMorphisms Isomorphism (makeGraph v1) (makeGraph v2) :: [LabeledMorphism]))

      it "produces v1! isomorphisms when v1 == v2" $
        forM_ [1..maxGraphSize] $ \v ->
        assertEqual ("with " ++ show (v, v))
          (foldl' (*) 1 [1..v])
          (length (findMorphisms Isomorphism (makeGraph v) (makeGraph v) :: [LabeledMorphism]))


    context "between graphs with one node and no labels" $ do
      let
        makeGraph e =
          fromNodesAndEdges [Node 0 Nothing] [ Edge x 0 0 () | x <- [0 .. toEnum (e - 1)] ]

      it "always produces e2^e1 morphisms" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..maxGraphSize] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          (e2 ^ e1)
          (length (findMorphisms GenericMorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces e2!/(e2-e1)! monomorphisms when e2 >= e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [e1..maxGraphSize] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          (foldl' (*) 1 $ take e1 [e2, e2-1 .. 1])
          (length (findMorphisms Monomorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces no monomorphisms when e2 < e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..e1-1] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          0
          (length (findMorphisms Monomorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces S(e1, e2) * e2! epimorphisms when e2 <= e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..e1] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          (fromInteger (stirling2nd e1 e2) * foldl' (*) 1 [1..e2])
          (length (findMorphisms Epimorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces no epimorphisms when e2 > e1" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [e1+1..maxGraphSize] $ \e2 ->
        assertEqual ("with " ++ show (e1, e2))
          0
          (length (findMorphisms Epimorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces no isomorphism when e1 /= e2" $
        forM_ [1..maxGraphSize] $ \e1 ->
        forM_ [0..maxGraphSize] $ \e2 ->
        when (e1 /= e2) $
        assertEqual ("with " ++ show (e1, e2))
          0
          (length (findMorphisms Isomorphism (makeGraph e1) (makeGraph e2) :: [LabeledMorphism]))

      it "produces v1! isomorphisms when e1 == e2" $
        forM_ [1..maxGraphSize] $ \e ->
        assertEqual ("with " ++ show (e, e))
          (foldl' (*) 1 [1..e])
          (length (findMorphisms Isomorphism (makeGraph e) (makeGraph e) :: [LabeledMorphism]))


    context "producing Monomorphims" $ modifyMaxSuccess (const 50) $ modifyMaxSize (const 20) $ do

      prop "always produces valid morphisms" $ \domain codomain ->
        forAllMorphismsBetween Monomorphism domain codomain $ \m ->
          isValid (m :: LabeledMorphism)

      prop "always produces monomorphisms" $ \domain codomain ->
        forAllMorphismsBetween Monomorphism domain codomain $ \m ->
          isMonomorphism (m :: LabeledMorphism)


    context "producing Epimorphisms" $ modifyMaxSuccess (const 20) $ modifyMaxSize (const 20) $ do

      prop "always produces valid morphisms" $ \codomain ->
        forAll (randomSubgraphOf codomain) $ \domain ->
        forAllMorphismsBetween Epimorphism domain codomain $ \m ->
          isValid (m :: LabeledMorphism)

      prop "always produces epimorphisms" $ \codomain ->
        forAll (randomSubgraphOf codomain) $ \domain ->
        forAllMorphismsBetween Epimorphism domain codomain $ \m ->
          isEpimorphism (m :: LabeledMorphism)


    context "producing Isomorphisms" $ modifyMaxSuccess (const 50) $ modifyMaxSize (const 25) $ do

      prop "always produces valid morphisms" $ \graph ->
        forAllMorphismsBetween Isomorphism graph graph $ \m ->
          isValid (m :: LabeledMorphism)

      prop "always produces isomorphisms" $ \graph ->
        forAllMorphismsBetween Isomorphism graph graph $ \m ->
          isIsomorphism (m :: LabeledMorphism)

      prop "always produces epimorphisms" $ \graph ->
        forAllMorphismsBetween Isomorphism graph graph $ \m ->
          isEpimorphism (m :: LabeledMorphism)

      prop "always produces monomorphisms" $ \graph ->
        forAllMorphismsBetween Isomorphism graph graph $ \m ->
          isMonomorphism (m :: LabeledMorphism)


    it "doesn't map labeled nodes to unlabeled nodes" $ do
      let g1 = fromNodesAndEdges [Node 0 (Just "x"), Node 1 (Just "y")] []
      let g2 = fromNodesAndEdges [Node 0 Nothing, Node 1 (Just "a"), Node 2 (Just "b")] []
      let morphisms = findMorphisms GenericMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 4
      forM_ morphisms $ \m -> do
        lookupNodeId 0 m `shouldNotBe` Just 0
        lookupNodeId 1 m `shouldNotBe` Just 0

    it "restricts the mapping of nodes with same label" $ do
      let g1 = fromNodesAndEdges [Node 0 (Just "x"), Node 1 (Just "x")] []
      let g2 = fromNodesAndEdges [Node 0 (Just "a"), Node 1 (Just "b")] []
      let morphisms = findMorphisms GenericMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 2
      forM_ morphisms $ \m ->
        lookupNodeId 0 m `shouldBe` lookupNodeId 1 m

    it "restricts the mapping of edges with same source" $ do
      let g1 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 0 1 ()]
      let g2 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 1 1 ()]
      let morphisms = findMorphisms GenericMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 2
      forM_ morphisms $ \m ->
        lookupEdgeId 0 m `shouldBe` lookupEdgeId 1 m

    it "restricts the mapping of edges with same target" $ do
      let g1 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 1 0 ()]
      let g2 = fromNodesAndEdges [Node 0 Nothing, Node 1 Nothing] [Edge 0 0 0 (), Edge 1 1 1 ()]
      let morphisms = findMorphisms GenericMorphism g1 g2 :: [LabeledMorphism]
      length morphisms `shouldBe` 2
      forM_ morphisms $ \m ->
        lookupEdgeId 0 m `shouldBe` lookupEdgeId 1 m



  describe "partialInjectiveMatches" $ do

    it "always produces valid morphisms" $
      property $ \g1 g2 g3 ->
        forAllMorphismsBetween GenericMorphism g2 g1 $ \left ->
        forAllMorphismsBetween GenericMorphism g2 g3 $ \right ->
        all isValid (partialInjectiveMatches left right :: [LabeledMorphism])

    it "always produces a commutative triangle" $
      property $ \g1 g2 g3 ->
        forAllMorphismsBetween GenericMorphism g2 g1 $ \left ->
        forAllMorphismsBetween GenericMorphism g2 g3 $ \right ->
        let
          commutes m = compose left m == right
        in
          all commutes (partialInjectiveMatches left right :: [LabeledMorphism])

    it "never collapses elements outside the image of the right morphism" $
      property $ \g1 g2 g3 ->
        forAllMorphismsBetween GenericMorphism g2 g1 $ \left ->
        forAllMorphismsBetween GenericMorphism g2 g3 $ \right ->
        (`all` partialInjectiveMatches left right) $ \f ->
        let
          neverCollapses elements applyTo =
            (`all` elements g1) $ \a ->
            (`all` elements g1) $ \b ->
            let
              inImageOfLeft x = any (\y -> applyTo y left == Just x) (elements g2)
            in
              a /= b -->
              not (inImageOfLeft a) || not (inImageOfLeft b) -->
              applyTo a f /= applyTo b f
        in
          neverCollapses nodeIds lookupNodeId
            && neverCollapses edgeIds lookupEdgeId
            && neverCollapses freeVariablesOf applyToVariable



(-->) :: Bool -> Bool -> Bool
False --> _ = True
True --> a = a

infixr 0 -->


forAllMorphismsBetween :: (QuickCheck.Testable prop, FindMorphism a, Show a) =>
  MorphismType -> Obj a -> Obj a -> (a -> prop) -> Property
forAllMorphismsBetween restriction g1 g2 f =
  let
    morphisms = findMorphisms restriction g1 g2
  in
    morphisms /= [] ==> forAll (elements morphisms) f
