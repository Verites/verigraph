module XML.GPRReader.GXLReaderSpec where

import           Data.List
import           Data.Matrix                     hiding ((<|>))
import           Test.Hspec

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.EssentialCriticalPairs
import           Category.TypedGraphRule
import qualified XML.GGXReader                   as XML
import qualified XML.GPRReader.GXLReader         as GPR

fileName1 = "tests/grammars/pacman2.ggx"
fileName2 = "tests/grammars/pacman.gps"
fileName3 = "tests/grammars/mutex2.ggx"
fileName4 = "tests/grammars/mutex.gps"
fileName5 = "tests/grammars/elevator.ggx"
fileName6 = "tests/grammars/elevator.gps"
fileName7 = "tests/grammars/elevatorWithFlags.gps"

dpoConf :: Category morph => MorphismsConfig morph
dpoConf = MorphismsConfig monic

spec :: Spec
spec = context "GPR Reader Test - CPA/CSA analysis is equal on GGX and GPR files" gprTest

gprTest :: Spec
gprTest = do
  it "Pacman grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName1 False dpoConf
    (ggGPR,_) <- GPR.readGrammar fileName2

    let (pacmanRulesGGX,pacmanRulesGPR,_) = getRules ggGGX ggGPR undefined

    runAnalysis findCriticalPairs pacmanRulesGPR pacmanRulesGGX
    runAnalysis findCriticalSequences pacmanRulesGPR pacmanRulesGGX

  it "Mutex grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName3 False dpoConf
    (ggGPR,_) <- GPR.readGrammar fileName4

    let (mutexRulesGGX,mutexRulesGPR,_) = getRules ggGGX ggGPR undefined

    runAnalysis findCriticalPairs mutexRulesGPR mutexRulesGGX
    runAnalysis findCriticalSequences mutexRulesGPR mutexRulesGGX

  it "Elevator grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName5 False dpoConf
    (ggGPR,_) <- GPR.readGrammar fileName6
    (ggGPRFlag,_) <- GPR.readGrammar fileName7

    let (elevatorRulesGGX,elevatorRulesGPR,elevatorRulesGPRFlag) = getRules ggGGX ggGPR ggGPRFlag

    runAnalysis (\conf _ -> findAllEssentialDeleteUse conf) elevatorRulesGPRFlag elevatorRulesGGX
    runAnalysis findCriticalPairs elevatorRulesGPR elevatorRulesGGX
    runAnalysis findCriticalSequences elevatorRulesGPR elevatorRulesGGX

runAnalysis algorithm rules1 rules2 =
  pairwise (algorithm dpoConf []) rules1 `shouldBe` pairwise (algorithm dpoConf []) rules2

getRules a b c = (f a, f b, f c)
  where
    f g = map snd (sortRules (productions g))
    sortRules = sortBy (\(a,_) (b,_) -> compare a b)

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
