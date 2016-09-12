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
 ~=? show (pairwise (findAllDeleteUse dpoConf) rules)],
       ["Test teseRodrigo Produce-Dangling "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (findAllProduceDangling dpoConf) rules)],
       ["Test teseRodrigo Produce-Forbid "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 2 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 1 0 0 )\n"
 ~=? show (pairwise (findAllProduceForbid dpoConf) rules)],
       ["Test teseRodrigo Produce-Use "++(printConf dpoConf)  ~:
         "( 0 1 0 0 0 0 0 0 )\n"++
         "( 0 0 3 0 0 0 0 0 )\n"++
         "( 0 0 0 1 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 1 0 2 0 0 0 )\n"
 ~=? show (pairwise (findAllProduceUse dpoConf) rules)],
       ["Test teseRodrigo Delete-Forbid "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (findAllDeleteForbid dpoConf) rules)],
        ["Test teseRodrigo Remove-Dangling "++(printConf dpoConf)  ~:
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (findAllRemoveDangling dpoConf) rules)],
 ["Test teseRodrigo Deliver-Delete "++(printConf dpoConf)  ~:
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 3 0 0 0 0 0 )\n"++
         "( 0 0 0 1 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 1 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 1 0 0 0 )\n"
 ~=? show (pairwise (findAllDeliverDelete dpoConf) rules)],
       ["Test teseRodrigo Deliver-Dangling "++(printConf dpoConf)  ~:
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 1 0 0 0 0 0 )\n"
 ~=? show (pairwise (findAllDeliverDangling dpoConf) rules)],
        ["Test teseRodrigo Forbid-Produce "++(printConf dpoConf)  ~:
         "( 0 3 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 1 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"++
         "( 0 0 0 0 0 0 0 0 )\n"
 ~=? show (pairwise (findAllForbidProduce dpoConf) rules)]]

tests dpoConf rules = test $
  (testTeseRodrigo (DPOConfig MonoMatches PartiallyMonomorphicNAC) rules) ++
  (testTeseRodrigo (DPOConfig MonoMatches MonomorphicNAC) rules) ++
  (testTeseRodrigo (DPOConfig AnyMatches PartiallyMonomorphicNAC) rules) ++
  (testTeseRodrigo (DPOConfig AnyMatches MonomorphicNAC) rules)

main :: IO Counts
main = do
  let fileName = "tests/grammars/teseRodrigo.ggx"
      dpoConf = DPOConfig MonoMatches PartiallyMonomorphicNAC
  (gg,_) <- XML.readGrammar fileName dpoConf

  let rules = map snd (GG.rules gg)

  runTestTT (tests dpoConf rules)

pairwise :: (a -> a -> [b]) -> [a] -> Matrix Int
pairwise f items =
  matrix (length items) (length items) $ \(i,j) ->
    length (f (items !! (i-1)) (items !! (j-1)))
