module XML.GPRReader.GXLReaderSpec where

import           Data.List
import           Data.Matrix                     hiding ((<|>))
import           Test.Hspec

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.EssentialCriticalPairs
import qualified Category.TypedGraph             as TGraph
import           Category.TypedGraphRule
import qualified Data.Graphs                     as Graph
import qualified XML.GGXReader                   as XML
import qualified XML.GPRReader.GXLReader         as GPR

fileName1 = "tests/grammars/pacman2.ggx"
fileName2 = "tests/grammars/pacman.gps"
fileName3 = "tests/grammars/mutex2.ggx"
fileName4 = "tests/grammars/mutex.gps"
fileName5 = "tests/grammars/elevator.ggx"
fileName6 = "tests/grammars/elevator.gps"
fileName7 = "tests/grammars/elevatorWithFlags.gps"

tGraphConfig = TGraph.TGraphConfig Graph.empty TGraph.AllMatches
tGRuleConfig = TGRuleConfig tGraphConfig TGraph.AllMatches

spec :: Spec
spec = context "GPR Reader Test - CPA/CSA analysis is equal on GGX and GPR files" gprTest

gprTest :: Spec
gprTest = do
  it "Pacman grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName1 False tGRuleConfig
    (ggGPR,_) <- GPR.readGrammar fileName2

    let (pacmanRulesGGX,pacmanRulesGPR,_) = getRules ggGGX ggGPR undefined

    runAnalysis tGraphConfig findCriticalPairs pacmanRulesGPR pacmanRulesGGX
    runAnalysis tGraphConfig findCriticalSequences pacmanRulesGPR pacmanRulesGGX

  it "Mutex grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName3 False tGRuleConfig
    (ggGPR,_) <- GPR.readGrammar fileName4

    let (mutexRulesGGX,mutexRulesGPR,_) = getRules ggGGX ggGPR undefined

    runAnalysis tGraphConfig findCriticalPairs mutexRulesGPR mutexRulesGGX
    runAnalysis tGraphConfig findCriticalSequences mutexRulesGPR mutexRulesGGX

  it "Elevator grammar" $ do
    (ggGGX,_,_) <- XML.readGrammar fileName5 False tGRuleConfig
    (ggGPR,_) <- GPR.readGrammar fileName6
    (ggGPRFlag,_) <- GPR.readGrammar fileName7

    let (elevatorRulesGGX,elevatorRulesGPR,elevatorRulesGPRFlag) = getRules ggGGX ggGPR ggGPRFlag

    runAnalysis tGraphConfig findAllEssentialDeleteUse elevatorRulesGPRFlag elevatorRulesGGX
    runAnalysis tGraphConfig findCriticalPairs elevatorRulesGPR elevatorRulesGGX
    runAnalysis tGraphConfig findCriticalSequences elevatorRulesGPR elevatorRulesGGX

runAnalysis :: TGraph.TGraphConfig n e -> (TypedGraphRule n e -> TypedGraphRule n e -> TGraph.TGraphCat n e [a]) -> [TypedGraphRule n e] -> [TypedGraphRule n e] -> IO ()
runAnalysis config algorithm rules1 rules2 =
  TGraph.runCat (pairwise algorithm rules1) config `shouldBe` TGraph.runCat (pairwise algorithm rules2) config

getRules a b c = (f a, f b, f c)
  where
    f g = map snd (sortRules (productions g))
    sortRules = sortBy (\(a,_) (b,_) -> compare a b)

pairwise :: Monad m => (a -> a -> m [b]) -> [a] -> m (Matrix Int)
pairwise f items =
  mapM (fmap length . uncurry f) $
  matrix (length items) (length items) (\(i,j) -> (items !! (i-1), items !! (j-1)))
