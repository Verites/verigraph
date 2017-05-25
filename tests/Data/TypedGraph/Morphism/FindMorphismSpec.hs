module Data.TypedGraph.Morphism.FindMorphismSpec where

import           Data.TypedGraph.Morphism.FindMorphismSpec.FindCospanCommuterTest
import           Data.TypedGraph.Morphism.FindMorphismSpec.FindMorphismsTest
import           Test.Hspec

spec :: Spec
spec = do
  context "findMorphism"
    findMorphismsTest

  context "findCospanCommuter"
    findCospanCommuterTest
