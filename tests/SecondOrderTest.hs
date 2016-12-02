import           Abstract.DPO
import           Test.HUnit
import qualified XML.GGXReader             as XML
import           Utils

-- | Checks if the minimalSafetyNACs was correctly generated.
tests log n = [all (== n) list ~=? True]
  where
    list = map snd log

main :: IO ()
main = do
  let fileName = "tests/grammars/nacs2rule.ggx"
      dpoConf1 = MorphismsConfig MonoMatches MonomorphicNAC
      dpoConf2 = MorphismsConfig AnyMatches MonomorphicNAC
  (_,log1) <- XML.readGrammar fileName False dpoConf1
  (_,log2) <- XML.readGrammar fileName False dpoConf2

  runTests $ tests log1 2 ++ tests log2 9
