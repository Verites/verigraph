module Processes
  ( Options
  , options
  , execute
  ) where

import           Abstract.Valid
import Data.Set (elemAt, toList)
import           GlobalOptions
import           Abstract.DPO
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

    let colimit = calculateRulesColimit $ head sequences
        conflictsAndDependencies = findConflictsAndDependencies colimit
        inducedByNacs = filterPotential conflictsAndDependencies

        ogg = generateOccurenceGrammar $ head sequences
        sgg = singleTypedGrammar ogg
        newRules = GG.rules . singleTypedGrammar $ ogg
        relation = concreteRelation ogg
        rulesRelation = filterRulesOccurenceRelation relation
        elementsRelation = filterElementsOccurenceRelation relation
        unique = (uniqueOrigin newRules)
    forM_ (zip sequences newRules) $ \((name, _, _), rules) ->
      when (null rules)
        (putStrLn $ "No graph process candidates were found for rule sequence '" ++ name ++ "'")

    putStrLn "Conflicts and Dependencies: "
    putStrLn $ show conflictsAndDependencies



    putStrLn "\n------------------\n"
    putStrLn $ "Conflicts and dependencies induced by NACs:\n " ++ show inducedByNacs

    putStrLn $ show $ map (isConcrete ogg) (toList inducedByNacs)

    putStrLn "\n##################\n"

    putStrLn "Rules Relation: "
    putStrLn $ show rulesRelation

    putStrLn "\n##################\n"

    putStrLn "Elements Relation: "
    putStrLn $ show elementsRelation

    putStrLn "\n##################\n"
    putStrLn "Tesing Validity\n"
    putStrLn $ "Are the origins and terminations of elements unique?\n>>> " ++ show unique

    putStrLn "\n------------------\n"
    putStrLn $ "Initial Graph is valid? \n>>> " ++ show (isValid $ GG.start sgg)
    putStrLn $ show (GG.start sgg)

    putStrLn "\n------------------\n"
    putStrLn $ "Is there a compatible concrete total order for rules?\n>>> " ++ show (findOrder rulesRelation)
    putStrLn $ "Is there a compatible concrete total order for elements?\n>>> " ++ show (findOrder elementsRelation)

    putStrLn "\n------------------\n"
    putStrLn "Is there a compatible concrete total order respecting NACs?\n>>> Undefined"

--    putStrLn "\n\n\n"
--    putStrLn $ show $ getUnderlyingDerivation (snd . head $ newRules)

    let newStart = GG.start sgg
        gg' = GG.grammar newStart [] newRules
    GW.writeGrammarFile (gg',gg2) ggName names (outputFile opts)
