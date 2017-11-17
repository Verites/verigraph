{-# LANGUAGE TupleSections #-}
module CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           Control.Monad                         (when)
import           Data.Matrix                           (Matrix)
import qualified Data.Matrix                           as Matrix
import           Data.Maybe                            (maybe)
import           Data.Monoid                           ((<>))
import qualified Data.Set                              as Set
import           GHC.Conc                              (numCapabilities)
import           Options.Applicative

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Analysis.CriticalPairs                (CriticalPair, findCriticalPairs,
                                                        isDeleteUse, isProduceDangling,
                                                        isProduceForbid)
import qualified Analysis.CriticalPairs                as CP
import           Analysis.CriticalSequence             (CriticalSequence,
                                                        findTriggeredCriticalSequences,
                                                        isDeleteForbid, isProduceUse,
                                                        isRemoveDangling)
import qualified Analysis.CriticalSequence             as CS
import           Analysis.EssentialCriticalPairs       (findEssentialCriticalPairs)
import           Analysis.Interlevel.EvolutionarySpans
import           Analysis.Interlevel.InterLevelCP
import           Category.TypedGraph                   (TypedGraphMorphism)
import           Category.TypedGraphRule               (RuleMorphism)
import           Data.TypedGraph                       (typeGraph)
import           GlobalOptions
import           Rewriting.DPO.TypedGraph              (emptyGraphRule)
import           Rewriting.DPO.TypedGraphRule
import           Util
import           Util.List
import qualified XML.GGXWriter                         as GW
import           XML.XMLUtilities                      (writeXML)

data Options = Options
  { outputFile    :: Maybe String
  , sndOrder      :: Bool
  , essentialFlag :: Bool
  , analysisType  :: AnalysisType
  }

data AnalysisType = Both | Conflicts | Dependencies deriving (Eq)

options :: Parser Options
options = Options
  <$> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("CPX file that will be written, receiving the critical pairs " ++
             "for the grammar (if absent, a summary will be printed to stdout)")))
  <*> flag False True
        ( long "snd-order"
        <> help "Set the analysis to the second-order rules")
  <*> flag False True
        ( long "essential"
        <> help "Compute the Essential Critical Pairs analysis (Warning: not fully supported yet)")
  <*> ( flag' Conflicts
          ( long "conflicts-only"
            <> help "Restrict to Critical Pair analysis")
      <|> flag' Dependencies
            ( long "dependencies-only"
              <> help "Restrict to Critical Sequence analysis")
      <|> pure Both)


execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    putStrLn $ "number of cores: " ++ show numCapabilities ++ "\n"

    let dpoConf = morphismsConf globalOpts
        dpoConf' = toSndOrderMorphismsConfig dpoConf
    case arbitraryMatches globalOpts of
      MonoMatches -> putStrLn "Only injective matches allowed."
      AnyMatches  -> putStrLn "Non-injective matches allowed."

    when (essentialFlag opts) $
      putStrLn "WARNING: essential critical pairs not fully supported"
    when (essentialFlag opts && useConstraints globalOpts) $
      putStrLn "WARNING: constraints are not supported for essential critical pairs"
    putStrLn ""

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""
    if sndOrder opts
      then do
        when (useConstraints globalOpts) $
          putStrLn "WARNING: Constraints are not supported for second-order rules"

        (fstOrderGG, sndOrderGG, ggName, names) <- loadSndOrderGrammar globalOpts True
        let fstOrderGG' = appendSndOrderConfsDeps fstOrderGG
            appendSndOrderConfsDeps = case analysisType opts of
              Conflicts -> appendSndOrderConflicts dpoConf' sndOrderGG
              Dependencies -> appendSndOrderDependencies dpoConf' sndOrderGG
              Both -> appendSndOrderDependencies dpoConf' sndOrderGG . appendSndOrderConflicts dpoConf' sndOrderGG

        processFirstOrderGrammar globalOpts opts dpoConf fstOrderGG' sndOrderGG ggName names

        putStrLn "Inter-level Critical Pairs Analysis"
        putStrLn "This log shows a list of (first-order rule, second-order rule) that are in conflict:"
        print $ Set.fromList
          [ (x,y)
            | sndOrdRule <- productions sndOrderGG
            , fstOrdRule <- productions fstOrderGG
            , (x,y,_,_) <- interLevelCP dpoConf' sndOrdRule fstOrdRule ]
        putStrLn ""

        let evoConflicts = allEvolSpans dpoConf' (productions sndOrderGG)
        putStrLn "Evolutionary Spans Inter-level CP:"
        putStrLn "This log shows pairs of (second-order rule, second-order rule, number of FolFol, number of ConfFol, number of FolConf, number of ConfConf)"
        print (printEvoConflicts evoConflicts)

      else do
        (fstOrderGG, ggName, names) <- loadGrammar globalOpts
        let tGraph = typeGraph . leftObject . snd . head $ productions fstOrderGG
        let emptySndOrderGG = grammar (emptyGraphRule tGraph) [] []
        processFirstOrderGrammar globalOpts opts dpoConf fstOrderGG emptySndOrderGG ggName names

    putStrLn ""
    putStrLn "Critical Pair Analysis done!"

processFirstOrderGrammar :: GlobalOptions -> Options -> MorphismsConfig (TypedGraphMorphism b c) -> Grammar (TypedGraphMorphism b c) -> Grammar (RuleMorphism b c) -> String -> [(String, String)] -> IO ()
processFirstOrderGrammar globalOpts opts dpoConf fstOrderGrammar sndOrderGrammar ggName names = do
  let
    (conflicts, dependencies) =
      case analysisType opts of
        Conflicts -> (Just conflicts', Nothing)
        Dependencies -> (Nothing, Just dependencies')
        Both -> (Just conflicts', Just dependencies')
    constrs
      | useConstraints globalOpts = constraints fstOrderGrammar
      | otherwise = []
    conflicts' = pairwiseCompareIntoMatrix (findConflicts dpoConf constrs) (productions fstOrderGrammar)
    findConflicts
      | essentialFlag opts = \conf _ -> findEssentialCriticalPairs conf
      | otherwise          = findCriticalPairs
    dependencies' = pairwiseCompareIntoMatrix (findTriggeredCriticalSequences dpoConf constrs) (productions fstOrderGrammar)

  case outputFile opts of
    Just file ->
      do
        putStrLn "Warning: exporting conflicts/dependencies to .cpx not fully supported."
        let conflicts' = maybe [] Matrix.toList conflicts
        let dependencies' = maybe [] Matrix.toList dependencies
        _ <- writeXML (GW.writeCpx (fstOrderGrammar, sndOrderGrammar) conflicts' dependencies' ggName names) file
        -- TODO: error handling for XML writes
        return ()
    Nothing -> do
      mapM_ (printConflicts (essentialFlag opts)) conflicts
      mapM_ printDependencies dependencies

-- | Evolutionary Spans to Strings
printEvoConflicts :: [(String, String, [EvoSpan a b])] -> [String]
printEvoConflicts = map printOneEvo
  where
    printOneEvo (r1, r2, x) = "(" ++ r1 ++ ", " ++ r2 ++ ", " ++
                       show (printConf (False,False) x) ++ ", " ++
                       show (printConf (True,False) x) ++ ", " ++
                       show (printConf (False,True) x) ++ ", " ++
                       show (printConf (True,True) x) ++ ")"
    printConf str evos = countElement str (map cpe evos)


printConflicts :: Bool -> Matrix (String, String, [CriticalPair morph]) -> IO ()
printConflicts isEssential conflicts' = do
  printMatrixLengths (essential ++ "Delete-Use") (filterMatrix isDeleteUse conflicts)
  printMatrixLengths (essential ++ "Produce-Dangling") (filterMatrix isProduceDangling conflicts)
  printMatrixLengths (essential ++ "Produce-Forbid") (filterMatrix isProduceForbid conflicts)
  printMatrixLengths (essential ++ "Conflicts") conflicts
  where
    conflicts = fmap (\(_,_,l) -> l) conflicts'
    essential
      | isEssential = "Essential "
      | otherwise   = ""

printDependencies :: Matrix (String, String, [CriticalSequence morph]) -> IO ()
printDependencies dependencies' = do
  printMatrixLengths "Produce-Use" (filterMatrix isProduceUse dependencies)
  printMatrixLengths "Remove-Dangling" (filterMatrix isRemoveDangling dependencies)
  printMatrixLengths "Delete-Forbid" (filterMatrix isDeleteForbid dependencies)
  printMatrixLengths "Dependencies" dependencies
  putStrLn ""
  where
    dependencies = fmap (\(_,_,l) -> l) dependencies'


filterMatrix :: Functor f => (a -> Bool) -> f [a] -> f [a]
filterMatrix pred = fmap (filter pred)

printMatrixLengths :: String -> Matrix [a] -> IO ()
printMatrixLengths title matrix = do
  putStrLn (title ++ ":")
  print (fmap length matrix)

pairwiseCompareIntoMatrix :: (a -> a -> b) -> [(String, a)] -> Matrix (String, String, b)
pairwiseCompareIntoMatrix compare namedItems =
  Matrix.fromList (length namedItems) (length namedItems) (pairwiseCompareIntoList compare namedItems)

pairwiseCompareIntoList :: (a -> a -> b) -> [(String, a)] -> [(String, String, b)]
pairwiseCompareIntoList compare namedItems =
  parallelMap (uncurry compare') [ (x, y) | x <- namedItems, y <- namedItems ]
  where compare' (nameX, x) (nameY, y) = (nameX, nameY, compare x y)

appendSndOrderConflicts :: MorphismsConfig (RuleMorphism a b) -> Grammar (RuleMorphism a b) -> Grammar (TypedGraphMorphism a b) -> Grammar (TypedGraphMorphism a b)
appendSndOrderConflicts conf gg2 gg1 = newGG1
  where
    conflicts = pairwiseCompareIntoList (findCriticalPairs conf []) (productions gg2)
    matches = concatMap (\(n1,n2,c) -> map (\ol -> (n1, n2, CP.getCriticalPairType ol, codomain (fst (CP.getCriticalPairMatches ol)))) c) conflicts
    conflictRules = map (\(idx,(n1,n2,tp,rule)) -> ("conflict_" ++ show tp ++ "_" ++ n1 ++ "_" ++ n2 ++ "_" ++ show idx, rule)) (zip ([0..]::[Int]) matches)
    newGG1 = grammar (start gg1) [] (productions gg1 ++ conflictRules)

appendSndOrderDependencies :: MorphismsConfig (RuleMorphism a b) -> Grammar (RuleMorphism a b) -> Grammar (TypedGraphMorphism a b) -> Grammar (TypedGraphMorphism a b)
appendSndOrderDependencies conf gg2 gg1 = newGG1
  where
    conflicts = pairwiseCompareIntoList (findTriggeredCriticalSequences conf []) (productions gg2)
    matches = concatMap (\(n1,n2,c) -> map (\ol -> (n1, n2, CS.getCriticalSequenceType ol, codomain (fst (CS.getCriticalSequenceComatches ol)))) c) conflicts
    conflictRules = map (\(idx,(n1,n2,tp,rule)) -> ("dependency_" ++ show tp ++ "_" ++ n1 ++ "_" ++ n2 ++ "_" ++ show idx, rule)) (zip ([0..]::[Int]) matches)
    newGG1 = grammar (start gg1) [] (productions gg1 ++ conflictRules)

