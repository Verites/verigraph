module Processes
  ( Options
  , options
  , execute
  ) where

import           Abstract.Valid
import           GlobalOptions
import           Abstract.DPO
import           Abstract.Morphism
import           Analysis.Processes
import           Control.Monad
import qualified Grammar.Core        as GG
import           Options.Applicative
import           TypedGraph.DPO.GraphProcess
import qualified XML.GGXReader            as XML
import qualified XML.GGXWriter            as GW

data Options = Options
  { outputFile     :: String }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the concurrent rules to the original graph grammar")

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = morphismsConf globalOpts

    (gg,gg2,_) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)
    sequences <- XML.readSequencesWithObjectFlow gg (inputFile globalOpts)

    let abc = calculateRulesColimit $ head sequences
        conflictsAndDependencies = findConflictsAndDependencies abc
        newGG = myGraphProcess $ head sequences
        newRules = generateGraphProcess $ head sequences
        relation = occurenceRelation newRules
        rulesRelation = filterRulesOccurenceRelation relation
        elementsRelation = filterElementsOccurenceRelation relation
    forM_ (zip sequences newRules) $ \((name, _, _), rules) ->
      when (null rules)
        (putStrLn $ "No graph process candidates were found for rule sequence '" ++ name ++ "'")

    putStrLn "##################\n"
    putStrLn $ "Initial Graph is valid? \n> " ++ show (isValid $ GG.start newGG)
    putStrLn $ show (GG.start newGG)
    putStrLn "\n##################\n"


    putStrLn "Conflicts and Dependencies: "
    putStrLn (show conflictsAndDependencies)

    putStrLn "\n##################\n"

    putStrLn "Rules Relation: "
    putStrLn $ show (rulesRelation)

    putStrLn "\n##################\n"

    putStrLn "Elements Relation: "
    putStrLn $ show (elementsRelation)

    let newStart = codomain $ getLHS $ snd $ head newRules
        gg' = GG.grammar newStart [] newRules
    GW.writeGrammarFile (gg',gg2) ggName names (outputFile opts)
