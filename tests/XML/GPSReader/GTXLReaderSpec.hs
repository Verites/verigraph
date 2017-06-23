module XML.GPSReader.GTXLReaderSpec where

import           Data.List
import           Data.Matrix                      hiding ((<|>))
import           Test.Hspec

import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Category.TypedGraphRule.JointlyEpimorphisms
import qualified XML.GGXReader                    as XML
import qualified XML.GPSReader.GTXLReader         as GPS

fileName1 = "tests/grammars/pacman2.ggx"
fileName2 = "tests/grammars/pacman.gps"

dpoConf = MorphismsConfig AnyMatches MonomorphicNAC

spec :: Spec
spec = context "GPS Reader Test" gpsTest

gpsTest :: Spec
gpsTest = do
    it "Pacman grammar CPA analysis must be equal on GGX and GPS files" $ do
      (ggGGX,_,_) <- XML.readGrammar fileName1 False dpoConf
      ggGPS <- GPS.readGrammar fileName2
      
      let rulesGGX = map snd (sortRules (productions ggGGX))
          rulesGPS = map snd (sortRules (productions ggGPS))
          
          sortRules = sortBy (\(a,_) (b,_) -> compare a b)
      
      (pairwise (findCriticalPairs dpoConf) rulesGPS) `shouldBe` pairwise (findCriticalPairs dpoConf) rulesGGX
      (pairwise (findCriticalSequences dpoConf) rulesGPS) `shouldBe` pairwise (findCriticalSequences dpoConf) rulesGGX

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
