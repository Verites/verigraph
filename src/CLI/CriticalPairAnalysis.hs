module CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           Abstract.AdhesiveHLR      (EpiPairs)
import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.Interlevel.EvolutionarySpans
import           Analysis.Interlevel.InterLevelCP
import           Control.Monad (when)
import           Data.List.Utils
import           Data.Matrix               hiding ((<|>))
import           GlobalOptions
import           Options.Applicative
import           SndOrder.Rule
import qualified TypedGraph.GraphGrammar   as GG
import qualified XML.GGXReader             as XML
import qualified XML.GGXWriter             as GW

data Options = Options
  { outputFile   :: Maybe String
  , sndOrder     :: Bool
  , analysisType :: AnalysisType
  }

data AnalysisType = Both | Conflicts | Dependencies | None deriving (Eq)

options :: Parser Options
options = Options
  <$> optional (strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help ("CPX file that will be written, receiving the critical pairs " ++
             "for the grammar (if absent, a summary will be printed to stdout)")))
  <*> cpOrder
  <*> cpAnalysisType

cpOrder :: Parser Bool
cpOrder = flag False True
    ( long "snd-order"
    <> help "Set the analysis to the second order rules")

cpAnalysisType :: Parser AnalysisType
cpAnalysisType =
      flag' Conflicts
        ( long "conflicts-only"
          <> help "Restrict to Critical Pair analysis")
  <|> flag' Dependencies
        ( long "dependencies-only"
          <> help "Restrict to Critical Sequence analysis")
  <|> pure Both

calculateConflicts :: AnalysisType -> Bool
calculateConflicts flag = flag `elem` [Both,Conflicts]

calculateDependencies :: AnalysisType -> Bool
calculateDependencies flag = flag `elem` [Both,Dependencies]

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = dpoConfig globalOpts
    
    (gg,printNewNacs) <- XML.readGrammar (inputFile globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let action = analysisType opts
        secondOrder = sndOrder opts
        writer = defWriterFun secondOrder dpoConf action
        rules = map snd (GG.rules gg)
        rules2 = map snd (GG.sndOrderRules gg)

        interlevelCPs = applySecondOrder (interLevelCP dpoConf) (GG.rules gg) (GG.sndOrderRules gg)
        evoConflicts = allEvolSpans dpoConf (GG.sndOrderRules gg)


    putStrLn $ "injective satisfability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""
    
    when secondOrder $ mapM_ putStrLn (XML.printMinimalSafetyNacsLog dpoConf printNewNacs)
    
    putStrLn ""
    
    let fstOrderAnalysis = printAnalysis action dpoConf rules
        sndOrderAnalysis = printAnalysis action dpoConf rules2
    case outputFile opts of
      Just file -> writer gg ggName names file
      Nothing -> if secondOrder
                   then sndOrderAnalysis
                   else fstOrderAnalysis
    
    when secondOrder $
      if matchRestriction dpoConf == AnyMatches
        then mapM_ putStrLn $
          "Inter-level Critical Pairs Analysis:" :
          "(First Order Rule) (Sencond Order Rule) (Conflict Index)" :
          (map printILCP interlevelCPs)
        else putStrLn "Inter-level CP not defined for only injective matches"
    
    putStrLn ""
    
    when secondOrder $
      mapM_ putStrLn $
        "Evolutionary Spans Interlevel CP:" : (printEvoConflicts evoConflicts)
    
    putStrLn ""
    putStrLn "Critical Pair Analysis done!"

-- | Inter-level CP to Strings
printILCP :: (String, String, Int, InterLevelCP a b) -> String
printILCP (fstName, sndName, idx, _) =
  fstName ++ " " ++ sndName ++ " (id:" ++ show idx ++ ") ... (conflict omitted)"

-- | Evolutionary Spans to Strings
printEvoConflicts :: [(String, [EvoSpan a b])] -> [String]
printEvoConflicts evo = map printOneEvo evo
  where
    printOneEvo e = fst e ++ "\n" ++ (printEvos (snd e))
    -- FIX: test with CPE type, not with String
    printEvos evos =
      printConf "FolFol" evos ++
      printConf "DuseFol" evos ++
      printConf "FolDuse" evos ++
      printConf "DuseDuse" evos
    printConf str evos = str ++ " : " ++ show (countElem str (map (show . cpe) evos)) ++ "\n"

printAnalysis :: (EpiPairs m, DPO m) =>
  AnalysisType -> DPOConfig -> [Production m] -> IO ()
printAnalysis action dpoConf rules =
  let confMatrix = analysisMatrix dpoConf rules
        findAllDeleteUse findAllProduceDangling findAllProduceForbid
        "Delete-Use" "Produce-Dangling" "Produce-Forbid" "Conflicts"
      depMatrix = triDepMatrix ++ irrDepMatrix
      triDepMatrix = analysisMatrix dpoConf rules
        findAllProduceUse findAllRemoveDangling findAllDeleteForbid
        "Produce-Use" "Remove-Dangling" "Deliver-Forbid" "Triggereds Dependencies"
      irrDepMatrix = analysisMatrix dpoConf rules
        findAllDeliverDelete findAllDeliverDangling findAllForbidProduce
        "Deliver-Delete" "Deliver-Dangling" "Forbid-Produce" "Irreversibles Dependencies"
  in mapM_
       putStrLn $
       (if calculateConflicts action then confMatrix else [])
       ++ (if calculateDependencies action then depMatrix else [])

-- Receives functions and theirs names,
-- and returns they applicated to the rules
analysisMatrix :: (EpiPairs m, DPO m)
  => DPOConfig -> [Production m]
  -> (DPOConfig -> Production m -> Production m -> [cps])
  -> (DPOConfig -> Production m -> Production m -> [cps])
  -> (DPOConfig -> Production m -> Production m -> [cps])
  -> String -> String -> String -> String
  -> [String]
analysisMatrix dpoConf rules f1 f2 f3 n1 n2 n3 n4 =
  let f1Matrix = pairwiseCompare (f1 dpoConf) rules
      f2Matrix = pairwiseCompare (f2 dpoConf) rules
      f3Matrix = pairwiseCompare (f3 dpoConf) rules
      finalMatrix =
        liftMatrix3
          (\x y z -> x ++ y ++ z)
          f1Matrix f2Matrix f3Matrix

  in  [ n1 ++ ":"
      , show (length <$> f1Matrix)
      , ""
      , n2 ++ ":"
      , show (length <$> f2Matrix)
      , ""
      , n3 ++ ":"
      , show (length <$> f3Matrix)
      , ""
      , "All " ++ n4 ++ ":"
      , show (length <$> finalMatrix)
      , ""]

defWriterFun :: Bool -> DPOConfig -> AnalysisType
             -> GG.GraphGrammar a b -> String
             -> [(String,String)] -> String -> IO ()
defWriterFun secondOrder config t =
  case (secondOrder,t) of
    (False, Conflicts)    -> GW.writeConflictsFile config
    (False, Dependencies) -> GW.writeDependenciesFile config
    (False, Both)         -> GW.writeConfDepFile config
    (True, Conflicts)     -> GW.writeSndOderConflictsFile config
    (True, Dependencies)  -> GW.writeSndOderDependenciesFile config
    (True, Both)          -> GW.writeSndOderConfDepFile config
    (_, None)             -> GW.writeGrammarFile

-- | Combine three matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
liftMatrix3 f ma mb mc = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos) (mc!pos)

pairwiseCompare :: (a -> a -> b) -> [a] -> Matrix b
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) ->
    compare (items !! (i-1)) (items !! (j-1))
