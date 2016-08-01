import           Abstract.AdhesiveHLR      (EpiPairs)
import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Data.Matrix               hiding ((<|>))
import           Test.HUnit
import qualified TypedGraph.GraphGrammar   as GG
import qualified XML.GGXReader             as XML

printConf dpoConf =
  show (matchRestriction dpoConf) ++
  " " ++
  show (nacSatisfaction dpoConf)

testTeseRodrigo dpoConf rules =
      [["Test teseRodrigo Delete-Use "++(printConf dpoConf)  ~:
         "( 2 0 0 0 0 0 0 0 )\n"++
         "( 0 3 0 0 0 0 0 0 )\n"++
         "( 0 2 6 0 0 0 0 0 )\n"++
         "( 0 0 0 1 0 0 0 0 )\n"++
         "( 0 0 1 0 4 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (allDeleteUse dpoConf) rules)],
       ["Test teseRodrigo Produce-Dangling "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (allProduceDangling dpoConf) rules)],
       ["Test teseRodrigo Produce-Forbid "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 2 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 1 0 0 )\n"
 ~=? show (pairwise (allProduceForbid dpoConf) rules)],
       ["Test teseRodrigo Produce-Use "++(printConf dpoConf)  ~:
         "( 0 1 0 0 0 0 0 0 )\n"++
         "( 0 0 3 0 0 0 0 0 )\n"++
         "( 0 0 0 1 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 1 0 2 0 0 0 )\n"
 ~=? show (pairwise (allProduceUse dpoConf) rules)],
       ["Test teseRodrigo Delete-Forbid "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (allDeleteForbid dpoConf) rules)],
        ["Test teseRodrigo Remove-Dangling "++(printConf dpoConf)  ~:
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (allRemoveDangling dpoConf) rules)],
 ["Test teseRodrigo Deliver-Delete "++(printConf dpoConf)  ~:
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 3 0 0 0 0 0 )\n"++
         "( 0 0 0 1 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 1 0 0 0 )\n"
 ~=? show (pairwise (allDeliverDelete dpoConf) rules)],
       ["Test teseRodrigo Deliver-Dangling "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 1 0 0 0 0 0 )\n"
 ~=? show (pairwise (allDeliverDangling dpoConf) rules)],
        ["Test teseRodrigo Forbid-Produce "++(printConf dpoConf)  ~:
         "( 0 3 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 1 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (allForbidProduce dpoConf) rules)]]

tests dpoConf rules = test $
  (testTeseRodrigo (DPOConfig MonoMatches PartMonoNacSatisfaction) rules) ++
  (testTeseRodrigo (DPOConfig MonoMatches MonoNacSatisfaction) rules) ++
  (testTeseRodrigo (DPOConfig AnyMatches PartMonoNacSatisfaction) rules) ++
  (testTeseRodrigo (DPOConfig AnyMatches MonoNacSatisfaction) rules)

main :: IO Counts
main = do
  let fileName = "grammars/teseRodrigo.ggx"
  gg <- XML.readGrammar fileName

  let dpoConf = DPOConfig MonoMatches PartMonoNacSatisfaction
      rules = map snd (GG.rules gg)
  
  runTestTT (tests dpoConf rules)

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
