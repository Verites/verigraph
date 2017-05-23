module Processes
  ( Options
  , options
  , execute
  ) where


import           Control.Monad
import           Data.Maybe                                               (fromJust, isJust)
import           Data.Monoid                                              ((<>))
import           Data.Set                                                 (toList)
import           GlobalOptions
import           Options.Applicative

import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.Process                           hiding (productions)
import           Abstract.Valid
import           Analysis.Processes
import qualified Data.TypedGraph                                          as TG
import           Rewriting.TypedGraph.DPO.GraphProcess
import           Rewriting.TypedGraph.DPO.GraphProcess.OccurrenceRelation
import qualified XML.GGXReader                                            as XML
import qualified XML.GGXWriter                                            as GW

newtype Options = Options
  { outputFile     :: String }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the concurrent productions to the original graph grammar")

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
        newRules = productions . singleTypedGrammar $ ogg
        relation = concreteRelation completeOgg
        rulesRelation = filterRulesOccurrenceRelation relation
        elementsRelation = filterElementsOccurrenceRelation relation
        unique = uniqueOrigin newRules
        (rulesNames, elementsNames) = getElements completeOgg
    forM_ (zip sequences newRules) $ \((name, _, _), productions) ->
      when (null productions)
        (putStrLn $ "No graph process candidates were found for rule sequence '" ++ name ++ "'")

    if null sequences then error "input file must have at least a rule sequence" else print ""

    let analysis =
            "Conflicts and Dependencies: {\n"
            ++ show  (eliminateSelfConflictsAndDependencies conflictsAndDependencies) ++ "\n}\n"
            ++ "Creation and Deletion Relation: {\n"
            ++ show (originRelation completeOgg) ++"\n}\n"
            ++ "Conflicts and dependencies induced by NACs: \n{"
            ++ show (map (findConcreteTrigger completeOgg) (toList inducedByNacs)) ++ "\n}\n"

        rulesOrdering = findOrder rulesRelation rulesNames
        elementsOrdering = findOrder elementsRelation elementsNames

        testCases = "Rules involved: {\n"
          ++ show rulesNames ++ "\n}\n"
          ++ "Concrete Rules Relation: {\n"
          ++ relationToString rulesRelation ++ "\n}\n"
          ++ "Elements involved: {\n"
          ++ show elementsNames ++ "\n}\n"
          ++ "Elements Relation: {\n"
          ++ relationToString elementsRelation ++ "\n}\n"
          ++ "\n\n"
          ++ "Rules Ordering: {"
          ++ show rulesOrdering ++ "\n}\n"
          ++ "Element Ordering: {"
          ++ show elementsOrdering ++"\n}\n\n"
          ++ "Set of Category Restrictions: {\n"
          ++ restrictionToString (restrictRelation completeOgg) ++ "\n}"

    putStrLn "Testing Serialization: "
    if unique
      then putStrLn "[OK] Unique creations and deletions"
      else putStrLn "[FAIL] At least one element is created or deleted for more than one rule"

    if isValid (initialGraph completeOgg)
      then putStrLn "[OK] Initial graph is valid"
      else putStrLn $ "[FAIL] Initial graph is not valid: \n"
                    ++ fromJust (errorMessages $ validate $ initialGraph completeOgg)
                    ++ "\n" ++ show (initialGraph completeOgg)

    if isValid (finalGraph completeOgg)
      then putStrLn "[OK] Final graph is valid"
      else putStrLn $ "[FAIL] Final graph is not valid: \n"
                    ++ fromJust (errorMessages $ validate $ finalGraph completeOgg)
                    ++ "\n" ++ show (finalGraph completeOgg)

    if isJust rulesOrdering
      then putStrLn "[OK] Concrete occurrence relation is a total order"
      else putStrLn "[FAIL] Concrete occurrence relation is not a total order"

    if isJust elementsOrdering
      then putStrLn "[OK] Concrete elements relation is a total order"
      else putStrLn "[FAIL] Concrete elements relation is not a total order"

    if emptyRestrictions completeOgg
      then putStrLn "[OK] There are no abstract restrictions"
      else putStrLn "[WARN] There are abstract restrictions"

    writeFile (outputFile opts ++ "_analysis") analysis
    putStrLn $ "Analysis written in " ++ (outputFile opts ++ "_analysis")

    writeFile (outputFile opts ++ "_test_cases") testCases
    putStrLn $ "Test cases written in " ++ (outputFile opts ++ "_test_cases")

    let newStart = start sgg
        gg' = addReachableGraphs (reachableGraphs sgg) (grammar newStart [] newRules)
    GW.writeGrammarFile (gg',gg2) ggName (buildNewNames names (doubleType completeOgg)) (outputFile opts ++ ".ggx")

buildNewNames :: [(String,String)] -> TG.TypedGraph a b -> [(String,String)]
buildNewNames oldNames tg = newNs ++ newEs
  where
    ns = map (\(n,t) -> (n, "I" ++ show t)) (TG.typedNodes tg)
    es = map (\(e,_,_,t) -> (e, "I" ++ show t)) (TG.typedEdges tg)
    newNs = map (\(n,it) -> ("I" ++ show n, rename (show n,it))) ns
    newEs = map (\(e,it) -> ("I" ++ show e, rename (show e,it))) es
    rename (z,it) = (\(x,y) -> x ++ "-" ++ z ++ y) (break (=='%') (find it))
    find it = fromJust (lookup it oldNames)
