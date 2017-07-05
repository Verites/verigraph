module XML.GPRReader.GXLReaderSpec where

import           Data.List
import           Data.Matrix                                 hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.EssentialCriticalPairs
import           Category.TypedGraphRule.JointlyEpimorphisms
import qualified XML.GGXReader                               as XML
import qualified XML.GPRReader.GXLReader                     as GPR

fileName1 = "tests/grammars/pacman2.ggx"
fileName2 = "tests/grammars/pacman.gps"
fileName3 = "tests/grammars/mutex2.ggx"
fileName4 = "tests/grammars/mutex.gps"
fileName5 = "tests/grammars/elevator.ggx"
fileName6 = "tests/grammars/elevator.gps"
fileName7 = "tests/grammars/elevatorWithFlags.gps"

dpoConf = MorphismsConfig AnyMatches MonomorphicNAC

spec :: Spec
spec = context "GPR Reader Test - CPA/CSA analysis is equal on GGX and GPR files" gprTest

gprTest :: Spec
gprTest = do
  it "Pacman grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName1 False dpoConf
    (ggGPR,_) <- GPR.readGrammar fileName2

    let (rulesGGX,rulesGPR,_) = getRules ggGGX ggGPR undefined

    pairwise (findCriticalPairs dpoConf) rulesGPR `shouldBe` pairwise (findCriticalPairs dpoConf) rulesGGX
    pairwise (findCriticalSequences dpoConf) rulesGPR `shouldBe` pairwise (findCriticalSequences dpoConf) rulesGGX

  it "Mutex grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName3 False dpoConf
    (ggGPR,_) <- GPR.readGrammar fileName4

    let (rulesGGX,rulesGPR,_) = getRules ggGGX ggGPR undefined

    pairwise (findCriticalPairs dpoConf) rulesGPR `shouldBe` pairwise (findCriticalPairs dpoConf) rulesGGX
    pairwise (findCriticalSequences dpoConf) rulesGPR `shouldBe` pairwise (findCriticalSequences dpoConf) rulesGGX

  it "Elevator grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName5 False dpoConf
    (ggGPR,_) <- GPR.readGrammar fileName6
    (ggGPRFlag,_) <- GPR.readGrammar fileName7

    let (rulesGGX,rulesGPR,rulesGPRFlag) = getRules ggGGX ggGPR ggGPRFlag

    pairwise (findAllEssentialDeleteUse dpoConf) rulesGPRFlag `shouldBe` pairwise (findAllEssentialDeleteUse dpoConf) rulesGGX
    pairwise (findCriticalPairs dpoConf) rulesGPR `shouldBe` pairwise (findCriticalPairs dpoConf) rulesGGX
    pairwise (findCriticalSequences dpoConf) rulesGPR `shouldBe` pairwise (findCriticalSequences dpoConf) rulesGGX

getRules a b c = (f a, f b, f c)
  where
    f g = map snd (sortRules (productions g))
    sortRules = sortBy (\(a,_) (b,_) -> compare a b)

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
