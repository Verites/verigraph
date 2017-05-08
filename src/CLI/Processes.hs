module Processes
  ( Options
  , options
  , execute
  ) where

import           Abstract.DPO
import           Abstract.DPO.Process
import           Abstract.Valid
import           Analysis.Processes
import           Control.Monad
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import           Data.Set                    (toList)
import           GlobalOptions
import qualified Grammar.Core                as GG
import           Options.Applicative
import           TypedGraph.DPO.GraphProcess
import           TypedGraph.DPO.OccurenceRelation
import qualified TypedGraph.Graph            as TG
import qualified XML.GGXReader               as XML
import qualified XML.GGXWriter               as GW

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
        inducedByNacs = filterInducedByNacs (eliminateSelfConflictsAndDependencies conflictsAndDependencies)

        ogg = generateDoublyTypedGrammar $ head sequences
        sgg = singleTypedGrammar ogg
        completeOgg = calculateNacRelations ogg inducedByNacs
        newRules = GG.rules . singleTypedGrammar $ ogg
        relation = concreteRelation completeOgg
        rulesRelation = filterRulesOccurrenceRelation relation
        elementsRelation = filterElementsOccurrenceRelation relation
        unique = (uniqueOrigin newRules)
        (rulesNames, elementsNames) = getElements completeOgg
    forM_ (zip sequences newRules) $ \((name, _, _), rules) ->
      when (null rules)
        (putStrLn $ "No graph process candidates were found for rule sequence '" ++ name ++ "'")

    if null sequences then error "input file must have at least a rule sequence" else print ""

    putStrLn "Conflicts and Dependencies: "
    print $ eliminateSelfConflictsAndDependencies conflictsAndDependencies

    putStrLn "\n##################\n"

    putStrLn "Creation and Deletion Relation: "
    print (originRelation completeOgg)

    putStrLn "\n------------------\n"
    putStrLn "Conflicts and dependencies induced by NACs:\n "

    print $ map (findConcreteTrigger completeOgg) (toList inducedByNacs)

    putStrLn "\n##################\n"

    putStrLn "All Rules:"
    print rulesNames
    putStrLn "\nRules Relation: "
    print rulesRelation

    putStrLn "\n##################\n"

    putStrLn "All Elements:"
    print elementsNames
    putStrLn "\nElements Relation: "
    print elementsRelation

    putStrLn "\n##################\n"
    putStrLn "Tesing Validity\n"
    putStrLn $ "Are the origins and terminations of elements unique?\n>>> " ++ show unique

    let validInitialGraph = isValid $ initialGraph completeOgg
    putStrLn "\n------------------\n"
    putStrLn $ "Is the initial graph valid? \n>>> " ++ show validInitialGraph
    if not validInitialGraph then putStrLn $ fromJust (errorMessages $ validate $ initialGraph completeOgg) else putStrLn ""
    print (initialGraph completeOgg)

    let validFinalGraph = isValid $ finalGraph ogg
    putStrLn "\n------------------\n"
    putStrLn $ "Is the final graph valid? \n>>> " ++ show validFinalGraph
    if not validFinalGraph then putStrLn $ fromJust (errorMessages $ validate $ finalGraph completeOgg) else putStrLn ""
    print (finalGraph completeOgg)

    putStrLn "\n------------------\n"
    putStrLn $ "Is there a compatible concrete total order for rules?\n>>> " ++ show (findOrder rulesRelation rulesNames)
    putStrLn $ "Is there a compatible concrete total order for elements?\n>>> " ++ show (findOrder elementsRelation elementsNames)

    putStrLn "\n------------------\n"
    putStrLn $ "Set of Restrictions:" ++ show (restrictRelation completeOgg)
    putStrLn $ "Is there a compatible concrete total order respecting NACs?\n>>> "
                ++ if emptyRestrictions completeOgg then "True" else "undefined"

    let newStart = GG.start sgg
        gg' = GG.addReachableGraphs (GG.reachableGraphs sgg) (GG.grammar newStart [] newRules)
    GW.writeGrammarFile (gg',gg2) ggName (buildNewNames names (doubleType completeOgg)) (outputFile opts)

buildNewNames :: [(String,String)] -> TG.TypedGraph a b -> [(String,String)]
buildNewNames oldNames tg = newNs ++ newEs
  where
    ns = map (\(n,t) -> (n, "I" ++ show t)) (TG.typedNodes tg)
    es = map (\(e,_,_,t) -> (e, "I" ++ show t)) (TG.typedEdges tg)
    newNs = map (\(n,it) -> ("I" ++ show n, rename (show n,it))) ns
    newEs = map (\(e,it) -> ("I" ++ show e, rename (show e,it))) es
    rename (z,it) = (\(x,y) -> x ++ "-" ++ z ++ y) (break (=='%') (find it))
    find it = fromJust (lookup it oldNames)
