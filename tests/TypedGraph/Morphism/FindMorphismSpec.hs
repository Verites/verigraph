module TypedGraph.Morphism.FindMorphismSpec where

import           Test.Hspec
import           TypedGraph.Morphism.FindMorphismSpec.FindCospanCommuterTest
import           TypedGraph.Morphism.FindMorphismSpec.FindMorphismsTest
spec :: Spec
spec = do
  context "findMorphism"
    findMorphismsTest

  context "findCospanCommuter"
    findCospanCommuterTest
