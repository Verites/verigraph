module RunBenchs ( Options(..) , options, execute ) where

import           Control.DeepSeq
import           Criterion
import           Criterion.Report
import           Criterion.Types
import           Data.Monoid                  ((<>))
import           Data.Text.Lazy               (unpack)
import           Options.Applicative

import           Abstract.Rewriting.DPO
import           Analysis.ParallelIndependent
import           Data.TypedGraph.Morphism     as TGM
import           GlobalOptions
import           Rewriting.DPO.TypedGraph

newtype Options = Options { outputFile :: String }

type Rule = TypedGraphRule () ()
type LHS = TypedGraphMorphism () ()
type Match = TypedGraphMorphism () ()
type Alg = Rule -> Rule -> (Match , Match) -> Bool
type FileFormat = [(LHS, LHS, [(Match, Match)])]

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "OUTPUT-FILE"
    <> action "file"
    )


expandLHS :: LHS -> Rule
expandLHS lhs = buildProduction lhs lhs [] -- RHS can't be undefined because of strict evalutation

executeConds :: Alg -> FileFormat -> [[Bool]]
executeConds cond input' =
  do
    (leftRule, rightRule, matchesList) <- input'
    return $ do
      (leftMatch, rightMatch) <- matchesList
      return $!! (((cond $!! (expandLHS $!! leftRule)) $!! (expandLHS $!! rightRule)) $!! (leftMatch, rightMatch))

execute :: GlobalOptions -> Options -> IO ()
execute globalOptions localOptions = do
  let dpoConf = morphismsConf globalOptions

  file <- readFile $ inputFile globalOptions
  let input = force $ read file :: FileFormat

      benchAbovePullback = nf (executeConds abovePullback) input
      benchAboveMorphism = nf (executeConds (aboveMorphism dpoConf)) input
      benchAbovePbIso    = nf (executeConds abovePullbackAndIso) input
      benchBelowPullback = nf (executeConds belowPullback) input
      benchBelowMorphism = nf (executeConds (belowMorphism dpoConf)) input

  putStrLn "Above - Reflection of arrows (ESS-PI):"
  a <- benchmark' benchAbovePullback

  putStrLn "Above - Factorization of arrows (ESS-F-PI):"
  b <- benchmark' benchAboveMorphism

  putStrLn "Above - Isomorphism of pullbacks (PBK-PI):"
  c <- benchmark' benchAbovePbIso

  putStrLn "Below - Reflection of arrows (STD-PI):"
  d <- benchmark' benchBelowPullback

  putStrLn "Below - Factorization o arrows (STD-F-PI):"
  e <- benchmark' benchBelowMorphism

  templateDir <- getTemplateDir
  template' <- loadTemplate [templateDir] "default"

  report' <- formatReport [ a {reportNumber=0, reportName="Above - Reflection of arrows      (ESS-PI)"}
                          , b {reportNumber=1, reportName="Above - Factorization of arrows (ESS-F-PI)"}
                          , c {reportNumber=2, reportName="Above - Isomorphism of pullbacks  (PBK-PI)"}
                          , d {reportNumber=3, reportName="Below - Reflection of arrows      (STD-PI)"}
                          , e {reportNumber=4, reportName="Below - Factorization o arrows  (STD-F-PI)"}]
            template'

  writeFile (outputFile localOptions) (unpack report')
