module Rewriting.DPO.TypedGraphRuleSpec where

import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import qualified XML.GGXReader          as XML

fileName1, fileName2 :: FilePath
fileName1 = "tests/grammars/nacs2rule.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"

dpoConf1, dpoConf2 :: Category morph => MorphismsConfig morph
dpoConf1 = MorphismsConfig monic
dpoConf2 = MorphismsConfig anyMorphism

spec :: Spec
spec = context "Second-Order Minimal Safety NACs Test" msnTest

msnTest :: Spec
msnTest =
  it "Minimal Safety NACs amount is correct" $
    do (_,_,log1) <- XML.readGrammar fileName1 False dpoConf1
       (_,_,log2) <- XML.readGrammar fileName1 False dpoConf2
       (_,_,log3) <- XML.readGrammar fileName2 False dpoConf1
       (_,_,log4) <- XML.readGrammar fileName2 False dpoConf2

       checkMinimalSafetyNACs log1 2
       checkMinimalSafetyNACs log2 9
       checkMinimalSafetyNACs log3 0
       checkMinimalSafetyNACs log4 0

-- | Checks if the number of minimalSafetyNACs was correctly generated.
checkMinimalSafetyNACs log n =
  do
    let list = map snd log
    list `shouldMatchList` replicate (length list) n
