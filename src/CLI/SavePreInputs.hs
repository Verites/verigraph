module SavePreInputs
  ( Options
  , options
  , execute
  ) where

import           Control.Monad
import           Data.Monoid                        ((<>))
import           Options.Applicative

import           Abstract.Category.FinitaryCategory
import           Abstract.Rewriting.DPO
import           Analysis.ParallelIndependent
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           GlobalOptions
import           Rewriting.DPO.TypedGraph
import qualified XML.GGXReader                      as XML

data Options = Options
  { outputPath :: String
  , outputMode :: Bool
  }

type TGM = TypedGraphMorphism () ()
type Rule = TypedGraphRule () ()
type LHS = TypedGraphMorphism () ()
type Match = TypedGraphMorphism () ()
type FileFormat = [(LHS, LHS, [(Match, Match)])]

options :: Parser Options
options = Options
  <$> strOption
  ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file")
  <*> switch
  ( long "matches"
    <> short 'm'
    <> help "Save matches instead of epi pairs." )

getEpiPairs ::MorphismsConfig -> [Rule] -> FileFormat
getEpiPairs _ [] = []
getEpiPairs dpoConf (firstRule:rules) =
  do
    let getEpiPairs' :: MorphismsConfig -> Rule -> [Rule] -> FileFormat
        getEpiPairs' dpoConf' firstRule' rules' =
          do
            secondRule <- rules'
            return (getLHS firstRule, getLHS secondRule, findEpiPairs dpoConf' firstRule' secondRule)

        epiPairsFirstRule :: [(Match, Match)]
        epiPairsFirstRule = findEpiPairs dpoConf firstRule firstRule

    (getLHS firstRule, getLHS firstRule, epiPairsFirstRule)
      :  getEpiPairs' dpoConf firstRule rules
      ++ getEpiPairs dpoConf rules

getMatches :: MorphismsConfig -> TypedGraph () () -> [Rule] -> FileFormat
getMatches _ _ [] = []
getMatches dpoConf initialGraph (firstRule:rules) =
  do
    let getMatches' :: MorphismsConfig -> TypedGraph () () -> Rule -> [Rule] -> FileFormat
        getMatches' dpoConf' initialGraph' firstRule' rules' =
          do
            secondRule <- rules'
            let matches' :: [(Match, Match)]
                matches' = do
                  firstMatch <- findMorphisms (matchRestrictionToMorphismType $ matchRestriction dpoConf') (codomain $ getLHS firstRule') initialGraph' :: [TGM]
                  secondMatch <- findMorphisms (matchRestrictionToMorphismType $ matchRestriction dpoConf') (codomain $ getLHS secondRule) initialGraph' :: [TGM]
                  guard $ satisfyRewritingConditions dpoConf (firstRule, firstMatch) (secondRule, secondMatch)
                  return (firstMatch,secondMatch)

            return (getLHS firstRule', getLHS secondRule, matches')

        matchesFirstRule :: [(Match, Match)]
        matchesFirstRule = do
          match  <- findMorphisms (matchRestrictionToMorphismType $ matchRestriction dpoConf) (codomain $ getLHS firstRule) initialGraph :: [TGM]
          guard $ satisfyRewritingConditions dpoConf (firstRule, match) (firstRule, match)
          return (match, match)

    (getLHS firstRule, getLHS firstRule, matchesFirstRule)
      :  getMatches' dpoConf initialGraph firstRule rules
      ++ getMatches dpoConf initialGraph rules


execute :: GlobalOptions -> Options -> IO ()
execute globalOptions localOptions = do
  let dpoConf = morphismsConf globalOptions
      useConstrains = False

  (inputGrammar, _, _) <- XML.readGrammar (inputFile globalOptions) useConstrains dpoConf

  let initialGraph   = start inputGrammar
      rules          = map snd (productions inputGrammar)

      output = if outputMode localOptions
               then getMatches dpoConf initialGraph rules
               else getEpiPairs dpoConf rules

      outputSize = length $ concatMap (\(_,_,m) -> m) output

  putStrLn $ show outputSize ++ " pairs generated."
  writeFile (outputPath localOptions) (show output)
