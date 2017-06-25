module XML.GPRReader.GXLReaderSpec where

import           Data.List
import           Data.Matrix                      hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Category.TypedGraphRule.JointlyEpimorphisms
import qualified XML.GGXReader                    as XML
import qualified XML.GPRReader.GXLReader          as GPR

fileName1 = "tests/grammars/pacman2.ggx"
fileName2 = "tests/grammars/pacman.gps"
fileName3 = "tests/grammars/mutex2.ggx"
fileName4 = "tests/grammars/mutex.gps"

dpoConf = MorphismsConfig AnyMatches MonomorphicNAC

spec :: Spec
spec = context "GPR Reader Test - CPA/CSA analysis is equal on GGX and GPR files" gprTest

gprTest :: Spec
gprTest = do
    it "Pacman grammar" $ do
      (ggGGX,_,_) <- XML.readGrammar fileName1 False dpoConf
      (ggGPR,_) <- GPR.readGrammar fileName2
      
      let rulesGGX = map snd (sortRules (productions ggGGX))
          rulesGPR = map snd (sortRules (productions ggGPR))
          
          sortRules = sortBy (\(a,_) (b,_) -> compare a b)
      
      (pairwise (findCriticalPairs dpoConf) rulesGPR) `shouldBe` pairwise (findCriticalPairs dpoConf) rulesGGX
      (pairwise (findCriticalSequences dpoConf) rulesGPR) `shouldBe` pairwise (findCriticalSequences dpoConf) rulesGGX

    it "Mutex grammar" $ do
      (ggGGX,_,_) <- XML.readGrammar fileName3 False dpoConf
      (ggGPR,_) <- GPR.readGrammar fileName4
      
      let rulesGGX = map snd (sortRules (productions ggGGX))
          rulesGPR = map snd (sortRules (productions ggGPR))
          
          sortRules = sortBy (\(a,_) (b,_) -> compare a b)
      
      (pairwise (findCriticalPairs dpoConf) rulesGPR) `shouldBe` pairwise (findCriticalPairs dpoConf) rulesGGX
      (pairwise (findCriticalSequences dpoConf) rulesGPR) `shouldBe` pairwise (findCriticalSequences dpoConf) rulesGGX

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
