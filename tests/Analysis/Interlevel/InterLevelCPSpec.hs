module Analysis.Interlevel.InterLevelCPSpec where

import           Data.Maybe                         (fromMaybe)
import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.Interlevel.InterLevelCP
import           Category.TypedGraphRule            ()
import           Data.TypedGraph
import           Util.List
import qualified XML.GGXReader                      as XML

fileName = "tests/grammars/secondOrderMatchTest.ggx"

dpoConf1, dpoConf2 :: Category morph => MorphismsConfig morph
dpoConf1 = MorphismsConfig monic
dpoConf2 = MorphismsConfig anyMorphism

spec :: Spec
spec = context "Inter-level Critical Pairs Test" dangextTest

dangextTest :: Spec
dangextTest = do
    it "dangling extension generates expected values" $ do
      (gg1,_,_) <- XML.readGrammar fileName False dpoConf1

      checkDanglingExtension gg1

    it "it finds inter-level conflicts in expected situations" $ do
      (gg1,gg2,_) <- XML.readGrammar fileName False dpoConf1

      checkInterlevelConflict dpoConf1 dpoConf2 gg1 gg2

-- | Runs dangling extension to a specific rule and checks if there is
-- the correct number of messages and data nodes, all nodes and all edges
checkDanglingExtension gg1 =
  do
    let ruleC = getRule "ruleC" gg1

        dangGraph = codomain (danglingExtension (leftMorphism ruleC))

        [(_,typeOfMsg),(_,typeOfData)] = nodes (leftObject ruleC)

        msgsInDang = countElement typeOfMsg (map snd nods)
        dataInDang = countElement typeOfData (map snd nods)
        nods = nodes dangGraph
        edgs = edges dangGraph

    msgsInDang  `shouldBe` 3
    dataInDang  `shouldBe` 3
    length nods `shouldBe` 8
    length edgs `shouldBe` 8

-- | Checks if the inter-level conflicts algorithm finds conflicts in
-- expected situations. It does not check what is the conflict.
checkInterlevelConflict mono arbitrary gg1 gg2 =
  do
    let sendMsg = ("sendMsg", getRule "sendMsg" gg1)
        getData = ("getData", getRule "getData" gg1)
        receiveMsg = ("receiveMsg", getRule "receiveMsg" gg1)
        deleteMsg = ("deleteMsg", getRule "deleteMsg" gg1)
        ruleB = ("ruleB", getRule "ruleB" gg1)
        ruleC = ("ruleC", getRule "ruleC" gg1)
        ruleD = ("ruleD", getRule "ruleD" gg1)

        a = ("a", getRule "a" gg2)
        b = ("b", getRule "b" gg2)
        c = ("c", getRule "c" gg2)
        d = ("d", getRule "d" gg2)

    length (interLevelCP arbitrary a sendMsg) `shouldBe` 1
    length (interLevelCP arbitrary b sendMsg) `shouldBe` 0
    length (interLevelCP arbitrary c sendMsg) `shouldBe` 0
    length (interLevelCP arbitrary d sendMsg) `shouldBe` 0
    length (interLevelCP arbitrary a getData) `shouldBe` 0
    length (interLevelCP arbitrary b getData) `shouldBe` 0
    length (interLevelCP arbitrary c getData) `shouldBe` 0
    length (interLevelCP arbitrary d getData) `shouldBe` 0
    length (interLevelCP arbitrary a receiveMsg) `shouldBe` 0
    length (interLevelCP arbitrary b receiveMsg) `shouldBe` 0
    length (interLevelCP arbitrary c receiveMsg) `shouldBe` 0
    length (interLevelCP arbitrary d receiveMsg) `shouldBe` 0
    length (interLevelCP arbitrary a deleteMsg) `shouldBe` 0
    length (interLevelCP arbitrary b deleteMsg) `shouldBe` 0
    length (interLevelCP arbitrary c deleteMsg) `shouldBe` 0
    length (interLevelCP arbitrary d deleteMsg) `shouldBe` 0
    length (interLevelCP arbitrary a ruleB) `shouldBe` 0
    Prelude.null (interLevelCP mono b ruleB) `shouldBe` False
    length (interLevelCP arbitrary c ruleB) `shouldBe` 0
    length (interLevelCP arbitrary d ruleB) `shouldBe` 0
    length (interLevelCP arbitrary a ruleC) `shouldBe` 0
    length (interLevelCP arbitrary b ruleC) `shouldBe` 0
    Prelude.null (interLevelCP mono c ruleC) `shouldBe` False
    length (interLevelCP arbitrary d ruleC) `shouldBe` 0
    length (interLevelCP arbitrary a ruleD) `shouldBe` 0
    length (interLevelCP arbitrary b ruleD) `shouldBe` 0
    length (interLevelCP arbitrary c ruleD) `shouldBe` 0
    Prelude.null (interLevelCP mono d ruleD) `shouldBe` False

getRule str gg =
  fromMaybe
    (error ("secondOrderTest: " ++ str ++ " is not in secondOrderMatchTest.ggx"))
    (lookup str (productions gg))
