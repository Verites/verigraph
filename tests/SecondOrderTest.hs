import           Abstract.DPO
import           Abstract.Morphism
import           Analysis.Interlevel.InterLevelCP
import           Data.List.Utils                  (countElem)
import           Grammar.Core
import           Test.HUnit
import           TypedGraph.Graph
import           Utils
import qualified XML.GGXReader as XML

-- | Checks if the number of minimalSafetyNACs was correctly generated.
checkMinimalSafetyNACs log n = [all (== n) list ~=? True]
  where
    list = map snd log

-- | Runs dangling extension to a specific rule and checks if there is
-- the correct number of messages and data nodes, all nodes and all edges
checkDanglingExtension gg1 =
  [ msgsInDang  ~=? 3
  , dataInDang  ~=? 3
  , length nods ~=? 8
  , length edgs ~=? 8
  ]
  where
    ruleC = case lookup "ruleC" (rules gg1) of
          Just x -> x
          Nothing -> error "secondOrderTest: ruleC is not in secondOrderMatchTest.ggx"
    
    left = getLHS ruleC
    dangGraph = codomain (danglingExtension left)
    
    [(_,typeOfMsg),(_,typeOfData)] = typedNodes (codomain left)
    
    msgsInDang = countElem typeOfMsg (map snd nods)
    dataInDang = countElem typeOfData (map snd nods)
    nods = typedNodes dangGraph
    edgs = typedEdges dangGraph

main :: IO ()
main = do
  let fileName1 = "tests/grammars/nacs2rule.ggx"
      fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
      dpoConf1 = MorphismsConfig MonoMatches MonomorphicNAC
      dpoConf2 = MorphismsConfig AnyMatches MonomorphicNAC
  (_,_,log1) <- XML.readGrammar fileName1 False dpoConf1
  (_,_,log2) <- XML.readGrammar fileName1 False dpoConf2
  (gg1,_,log3) <- XML.readGrammar fileName2 False dpoConf2

  runTests $
    checkMinimalSafetyNACs log1 2 ++
    checkMinimalSafetyNACs log2 9 ++
    checkMinimalSafetyNACs log3 0 ++
    checkDanglingExtension gg1
