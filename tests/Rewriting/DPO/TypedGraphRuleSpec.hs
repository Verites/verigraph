module Rewriting.DPO.TypedGraphRuleSpec where

import           Test.Hspec

import qualified Category.TypedGraph     as TGraph
import qualified Category.TypedGraphRule as TGRule
import           Data.Graphs             as Graph
import qualified XML.GGXReader           as XML

fileName1 = "tests/grammars/nacs2rule.ggx"
fileName2 = "tests/grammars/secondOrderMatchTest.ggx"

dpoConf1 = TGRule.Config (TGraph.Config Graph.empty TGraph.MonicMatches) TGRule.MonicMatches
dpoConf2 = TGRule.Config (TGraph.Config Graph.empty TGraph.AllMatches) TGRule.AllMatches

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
