-- | Test Suite for GraphProcess Module
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Category.TypedGraph.CocompleteSpec where

import           Control.Monad
import           Data.Proxy
import           Test.Hspec
import           Test.HUnit

import           Abstract.Category
import           Abstract.Category.Limit
import           Base.Valid
import           Category.TypedGraph      ()
import           Data.Graphs
import           Data.Graphs.Morphism
import           Data.List.NonEmpty       (fromList)
import           Data.TypedGraph.Morphism
import           Util.Test

tg :: TypeGraph
tg = makeTypeGraph ["N"] [("E", "N", "N")]

spec :: Spec
spec = do
  context "isInitial" $ do
    it "is true for initialObject" $
      let someMorphism = identity @GrMorphism (parseGraph tg "")
      in isInitial (Proxy @GrMorphism) (initialObject someMorphism) `shouldBe` True
    it "is true for an empty graph" $
      isInitial (Proxy @GrMorphism) (parseGraph tg "") `shouldBe` True
    it "is false for other graphs" $
      let graphs = map (parseGraph tg) ["n : N", "n1 n2: N", "n: N; n-:E-> n"]
      in forM_ graphs $ \graph ->
        isInitial (Proxy @GrMorphism) graph `shouldBe` False

  context "morphismFromInitialTo" $ do
    let graphs = map (parseGraph tg)
          ["", "n : N", "n1 n2: N", "n: N; n-:E-> n"]
    it "is valid" .
      forM_ graphs $ \graph ->
        validate (morphismFromInitialTo @GrMorphism graph) `shouldBe` IsValid
    it "has correct domain" .
      forM_ graphs $ \graph ->
        domain (morphismFromInitialTo @GrMorphism graph) `shouldBe` initialObject (identity @GrMorphism graph)
    it "has correct codomain" .
      forM_ graphs $ \graph ->
        codomain (morphismFromInitialTo @GrMorphism graph) `shouldBe` graph

  context "calculateCoproduct" .
    it "should produce a disjoint union" $ do
      let g = parseGraph tg "a : N; a-f:E->a"
      let h = parseGraph tg "b c : N; b-g:E->c"
      let (j1, j2) = calculateCoproduct @GrMorphism g h

      let gh = parseGraph tg "a b c : N; a-f:E->a; b-g:E->c"
      assertIsomorphic "codomain j1" gh (codomain j1)
      assertIsomorphic "j1" (parseMorphism g gh "a->a; f->f") j1
      assertEqual "codomain j2" (codomain j1) (codomain j2)
      assertIsomorphic "j2" (parseMorphism h gh "b->b; c->c; g->g") j2

  context "calculateCoequalizer" $
    it "Produces the expected result"
      coequalizerTests

  context "calculatePushout" $
    it "Produces the expected result"
      pushoutTests

  context "calculateNCoequalizer" $
    it "Produces the expected result"
      nCoequalizerTests

coequalizerTests :: Expectation
coequalizerTests = do
  testCaseOne
    `shouldBe`
    buildTypedGraphMorphism typedGraphBOne typedGraphBOne
    (buildGraphMorphism graphBOne graphBOne [(50,50),(60,60),(70,70),(80,80)] [])

  testCaseTwo
    `shouldBe`
    buildTypedGraphMorphism typedGraphBTwo typedGraphBTwo
    (buildGraphMorphism graphBTwo graphBTwo [(50,50),(60,60),(70,70)] [(200,200),(300,300)])

  testCaseThree
    `shouldBe`
    buildTypedGraphMorphism typedGraphBThree typedGraphXThree
    (buildGraphMorphism graphBThree graphXThree [(80,50),(70,50),( 60,50),(50,50)] [(200,200),(300,300),(400,400)])

  testCaseFour
    `shouldBe`
    buildTypedGraphMorphism typedGraphBFour typedGraphXFour
    (buildGraphMorphism graphBFour graphXFour [(50,50),(60,60)][(500,500),(600,500)])

pushoutTests :: Expectation
pushoutTests =
  testCaseThreePUSHOUT
    `shouldBeIsomorphicTo`
    pushoutResultThree

nCoequalizerTests :: Expectation
nCoequalizerTests = do
  testCaseFive
    `shouldBe`
    buildTypedGraphMorphism typedGraphBFive typedGraphXFive
    (buildGraphMorphism graphBFive graphXFive [(80,50),(70,50),(60,50),(50,50)] [])

  --Same result as test one
  testCaseSix
    `shouldBe`
    buildTypedGraphMorphism typedGraphBOne typedGraphBOne
    (buildGraphMorphism graphBOne graphBOne [(50,50),(60,60),(70,70),(80,80)] [])

  --Same result as test two
  testCaseSeven
    `shouldBe`
    buildTypedGraphMorphism typedGraphBTwo typedGraphBTwo
    (buildGraphMorphism graphBTwo graphBTwo [(50,50),(60,60),(70,70)] [(200,200),(300,300)])

  testCaseEight
    `shouldBe`
    buildTypedGraphMorphism typedGraphBEight typedGraphBEight
    (buildGraphMorphism graphBEight graphBEight [(50,50),(60,60),(70,70),(80,80)] [(500,500),(600,600),(700,700)])

-- | COEQUALIZER TESTS

-- |  Case 1
--   -h->
-- A     B --> X
--   -h->

typeGraphOne = build [1] []

graphAOne = build [10,20,30,40] []
graphBOne = build [50,60,70,80] []

typedGraphAOne = buildGraphMorphism graphAOne typeGraphOne [(10,1),(20,1),(30,1),(40,1)] []
typedGraphBOne = buildGraphMorphism graphBOne typeGraphOne [(50,1),(60,1),(70,1),(80,1)] []

mappingMorphismHOne = buildGraphMorphism graphAOne graphBOne [(10,50),(20,60),(30,70),(40,80)] []
typedMorphismHOne = buildTypedGraphMorphism typedGraphAOne typedGraphBOne mappingMorphismHOne

testCaseOne = calculateCoequalizer typedMorphismHOne typedMorphismHOne



-- | Case 2
-- Same diagram that case 1, but graphs have edges

typeGraphTwo = build [1] [(1,1,1)]

graphATwo = build [10,20] [(100,20,10)]
graphBTwo = build [50,60,70] [(200,60,50),(300,60,50)]

typedGraphATwo = buildGraphMorphism graphATwo typeGraphTwo [(10,1),(20,1)] [(100,1)]
typedGraphBTwo = buildGraphMorphism graphBTwo typeGraphTwo [(50,1),(60,1),(70,1)] [(200,1),(300,1)]

mappingMorphismHTwo = buildGraphMorphism graphATwo graphBTwo [(10,50),(20,60)] [(100,200)]

typedMorphismHTwo = buildTypedGraphMorphism typedGraphATwo typedGraphBTwo mappingMorphismHTwo

testCaseTwo = calculateCoequalizer typedMorphismHTwo typedMorphismHTwo



-- | Case 3 ( EXTRA TEST FOR PUSHOUT )


graphXThree = build [50] [(200,50,50),(300,50,50),(400,50,50)]
typedGraphXThree = buildGraphMorphism graphXThree typeGraphThree [(50,1)] [(200,1),(300,1),(400,1)]


graphXThreePUSHOUT = build [50,60,70,80,131] [(200,60,50),(300,70,60),(400,80,70),(601,50,131),(701,60,50),(801,70,60)]
typedGraphXThreePUSHOUT = buildGraphMorphism graphXThreePUSHOUT typeGraphThree [(50,1),(60,1),(70,1),(80,1),(131,1)] [(200,1),(300,1),(400,1),(601,1),(701,1),(801,1)]

mappingMorphismFThreePUSHOUT = buildGraphMorphism graphBThree graphXThreePUSHOUT [(50,131),(60,50),(70,60),(80,70)] [(200,601),(300,701),(400,801)]
mappingMorphismGThreePUSHOUT = buildGraphMorphism graphBThree graphXThreePUSHOUT [(50,50),(60,60),(70,70),(80,80)] [(200,200),(300,300),(400,400)]

typedMorphismFThreePUSHOUT = buildTypedGraphMorphism typedGraphBThree typedGraphXThreePUSHOUT mappingMorphismFThreePUSHOUT
typedMorphismGThreePUSHOUT = buildTypedGraphMorphism typedGraphBThree typedGraphXThreePUSHOUT mappingMorphismGThreePUSHOUT

pushoutResultThree = (typedMorphismFThreePUSHOUT, typedMorphismGThreePUSHOUT)

------------------------------------------------------------------------------------

typeGraphThree = build [1] [(1,1,1)]

graphAThree = build [10,20,30] []
graphBThree = build [50,60,70,80] [(200,60,50),(300,70,60),(400,80,70)]

typedGraphAThree = buildGraphMorphism graphAThree typeGraphThree [(10,1),(20,1),(30,1)] []
typedGraphBThree = buildGraphMorphism graphBThree typeGraphThree [(50,1),(60,1),(70,1),(80,1)] [(200,1),(300,1),(400,1)]

mappingMorphismFThree = buildGraphMorphism graphAThree graphBThree [(10,50),(20,60),(30,70)] []
mappingMorphismGThree = buildGraphMorphism graphAThree graphBThree [(10,80),(20,50),(30,60)] []
mappingMorphismHThree = buildGraphMorphism graphAThree graphBThree [(10,60),(20,70),(30,80)] []

typedMorphismFThree = buildTypedGraphMorphism typedGraphAThree typedGraphBThree mappingMorphismFThree
typedMorphismGThree = buildTypedGraphMorphism typedGraphAThree typedGraphBThree mappingMorphismGThree
typedMorphismHThree = buildTypedGraphMorphism typedGraphAThree typedGraphBThree mappingMorphismHThree

testCaseThreePUSHOUT = calculatePushout typedMorphismFThree typedMorphismHThree

testCaseThree = calculateCoequalizer typedMorphismFThree typedMorphismGThree


-- | Case 4

typeGraphFour = build [1] [(1,1,1)]

graphAFour = build [10,20] [(100,20,10),(200,20,10),(300,20,10),(400,20,10)]
graphBFour = build [50,60] [(500,60,50),(600,60,50)]
graphXFour = build [50,60] [(500,60,50)]

typedGraphAFour = buildGraphMorphism graphAFour typeGraphFour [(10,1),(20,1)] [(100,1),(200,1),(300,1),(400,1)]
typedGraphBFour = buildGraphMorphism graphBFour typeGraphFour [(50,1),(60,1)] [(500,1),(600,1)]
typedGraphXFour = buildGraphMorphism graphXFour typeGraphFour [(50,1),(60,1)] [(500,1)]

mappingMorphismGFour = buildGraphMorphism graphAFour graphBFour [(10,50),(20,60)] [(100,600),(200,500),(300,600),(400,500)]
mappingMorphismHFour = buildGraphMorphism graphAFour graphBFour [(10,50),(20,60)] [(100,500),(200,500),(300,600),(400,500)]

typedMorphismGFour = buildTypedGraphMorphism typedGraphAFour typedGraphBFour mappingMorphismGFour
typedMorphismHFour = buildTypedGraphMorphism typedGraphAFour typedGraphBFour mappingMorphismHFour

testCaseFour = calculateCoequalizer typedMorphismHFour typedMorphismGFour




-- * N COEQUALIZER TESTS


-- | Test case 5

typeGraphFive = build [4,3,2,1] [(5,3,4),(4,2,4),(3,2,3),(2,2,1),(1,3,1)]

graphAFive = build [10,20,30,40] []
graphBFive = build [50,60,70,80] []
graphXFive = build [50] []

typedGraphAFive = buildGraphMorphism graphAFive typeGraphFive [(10,1),(20,1),(30,1),(40,1)] []
typedGraphBFive = buildGraphMorphism graphBFive typeGraphFive [(50,1),(60,1),(70,1),(80,1)] []
typedGraphXFive = buildGraphMorphism graphXFive typeGraphFive [(50,1)] []

mappingMorphismFFive = buildGraphMorphism graphAFive graphBFive [(10,60),(20,50),(30,70),(40,80)] []
mappingMorphismGFive = buildGraphMorphism graphAFive graphBFive [(10,50),(20,60),(30,70),(40,80)] []
mappingMorphismHFive = buildGraphMorphism graphAFive graphBFive [(10,50),(20,80),(30,60),(40,70)] []

typedMorphismFFive = buildTypedGraphMorphism typedGraphAFive typedGraphBFive mappingMorphismFFive
typedMorphismGFive = buildTypedGraphMorphism typedGraphAFive typedGraphBFive mappingMorphismGFive
typedMorphismHFive = buildTypedGraphMorphism typedGraphAFive typedGraphBFive mappingMorphismHFive

testCaseFive = calculateNCoequalizer $ fromList [typedMorphismFFive, typedMorphismGFive, typedMorphismHFive]



-- | See COEqualizer test case One
testCaseSix = calculateNCoequalizer $ fromList [typedMorphismHOne,typedMorphismHOne]



-- | See COEqualizer test case Two
testCaseSeven = calculateNCoequalizer $ fromList [typedMorphismHTwo,typedMorphismHTwo]



-- | Test case 8

typeGraphEight = build [1] [(1,1,1)]

graphAEight = build [10,20] [(100,20,10)]
graphBEight = build [50,60,70,80] [(500,60,50),(600,70,60),(700,80,70)]

typedGraphAEight = buildGraphMorphism graphAEight typeGraphEight [(10,1),(20,1)] [(100,1)]
typedGraphBEight = buildGraphMorphism graphBEight typeGraphEight [(50,1),(60,1),(70,1),(80,1)] [(500,1),(600,1),(700,1)]

mappingMorphismFEight = buildGraphMorphism graphAEight graphBEight [(10,50),(20,60)] [(100,500)]

typedMorphismFEight = buildTypedGraphMorphism typedGraphAEight typedGraphBEight mappingMorphismFEight

testCaseEight = calculateNCoequalizer $ fromList [typedMorphismFEight]
