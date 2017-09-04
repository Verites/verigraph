module Rewriting.DPO.TypedGraphRule.NacManipulationSpec where

import           Data.Maybe                                   (fromMaybe)
import           Test.Hspec

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Abstract.Rewriting.DPO
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraphRule.NacManipulation
import qualified XML.GGXReader                                as XML

fileName = "tests/grammars/NacManipulation.ggx"

dpoConf :: Category morph => MorphismsConfig morph
dpoConf = MorphismsConfig monic

spec :: Spec
spec = context "NAC Manipulation Test" nacmanipTest

nacmanipTest :: Spec
nacmanipTest =
  it "create/delete the expected number of NACs" $
    do (gg,_,_) <- XML.readGrammar fileName False dpoConf
       checkNacManipulation gg

-- | Checks if the NAC manipulations functions create/delete the
-- expected number of NACs
checkNacManipulation gg =
  do
    let find :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
        find x y = findAllMorphisms (codomain x) (codomain y)

        -- Creation
        creation_modeledNACs_rule = getRule "creation_modeledNACs" gg
        creation_concreteNACs_rule = getRule "creation_concreteNACs" gg

        match = head (find (leftMorphism creation_modeledNACs_rule) (leftMorphism creation_concreteNACs_rule))
        creation_modeledNACs = nacs creation_modeledNACs_rule

        createDisable = createStep DisableCreate match creation_modeledNACs
        createPO = createStep Pushout match creation_modeledNACs
        createShift = createStep ShiftNACs match creation_modeledNACs

        -- Deletion
        deletion_modeledNACs_rule = getRule "deletion_modeledNACs" gg
        deletion_concreteNACs_rule = getRule "deletion_concreteNACs" gg

        deletion_modeledNACs = nacs deletion_modeledNACs_rule
        deletion_concreteNACs = nacs deletion_concreteNACs_rule

        deleteDisable = deleteStep DisableDelete deletion_modeledNACs deletion_concreteNACs
        deleteMono = deleteStep Monomorphisms deletion_modeledNACs deletion_concreteNACs
        deleteIPO = deleteStep InitialPushouts deletion_modeledNACs deletion_concreteNACs

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
