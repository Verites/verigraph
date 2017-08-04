{-# LANGUAGE TypeFamilies #-}
module Rewriting.DPO.TypedGraphRule.NacManipulationSpec where

import           Data.Maybe                                   (fromMaybe)
import           Test.Hspec

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import qualified Category.TypedGraph                          as TGraph
import qualified Category.TypedGraphRule                      as TGRule
import qualified Data.Graphs                                  as Graph
import           Data.TypedGraph
import           Rewriting.DPO.TypedGraph
import           Rewriting.DPO.TypedGraphRule.NacManipulation
import qualified XML.GGXReader                                as XML

fileName = "tests/grammars/NacManipulation.ggx"
config = TGraph.Config Graph.empty TGraph.MonicMatches

spec :: Spec
spec = context "NAC Manipulation Test" nacmanipTest

nacmanipTest :: Spec
nacmanipTest =
  it "create/delete the expected number of NACs" $
    do (gg,_,_) <- XML.readGrammar fileName False (TGRule.Config config TGRule.MonicMatches)
       checkNacManipulation gg

-- | Checks if the NAC manipulations functions create/delete the
-- expected number of NACs
checkNacManipulation :: TypedGraphGrammar n e -> IO ()
checkNacManipulation gg = do
  let
    config' = config { TGraph.catTypeGraph = typeGraph (start gg) }
    (createDisable, createPO, createShift, deleteDisable, deleteMono, deleteIPO) = TGraph.runCat config' $ do
      let find x y = findMorphisms anyMorphism (codomain x) (codomain y)

      -- Creation
      let creation_modeledNACs_rule = getRule "creation_modeledNACs" gg
          creation_concreteNACs_rule = getRule "creation_concreteNACs" gg
      match <- head <$> find (leftMorphism creation_modeledNACs_rule) (leftMorphism creation_concreteNACs_rule)
      let creation_modeledNACs = nacs creation_modeledNACs_rule

      createDisable <- createStep DisableCreate match creation_modeledNACs
      createPO <- createStep Pushout match creation_modeledNACs
      createShift <- createStep ShiftNACs match creation_modeledNACs

      -- Deletion
      let deletion_modeledNACs_rule = getRule "deletion_modeledNACs" gg
          deletion_concreteNACs_rule = getRule "deletion_concreteNACs" gg
          deletion_modeledNACs = nacs deletion_modeledNACs_rule
          deletion_concreteNACs = nacs deletion_concreteNACs_rule

      deleteDisable <- deleteStep DisableDelete deletion_modeledNACs deletion_concreteNACs
      deleteMono <- deleteStep Monomorphisms deletion_modeledNACs deletion_concreteNACs
      deleteIPO <- deleteStep InitialPushouts deletion_modeledNACs deletion_concreteNACs
      return (createDisable, createPO, createShift, deleteDisable, deleteMono, deleteIPO)

  length createDisable `shouldBe` 0
  length createPO `shouldBe` 1
  length createShift `shouldBe` 3
  length deleteDisable `shouldBe` 3
  length deleteMono `shouldBe` 0
  length deleteIPO `shouldBe` 2

getRule str gg =
  fromMaybe
    (error ("secondOrderTest: " ++ str ++ " is not in secondOrderMatchTest.ggx"))
    (lookup str (productions gg))
