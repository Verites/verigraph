import           Abstract.DPO
import           Test.HUnit
import qualified XML.GGXReader             as XML

-- | Checks if the minimalSafetyNACs was correctly generated.
tests log = test ([all (== 9) list ~=? True])
  where
    list = map snd log

main :: IO Counts
main = do
  let fileName = "tests/grammars/nacs2rule.ggx"
      dpoConf = DPOConfig MonoMatches MonomorphicNAC
  (gg,log) <- XML.readGrammar fileName False dpoConf
  
  runTestTT (tests log)
