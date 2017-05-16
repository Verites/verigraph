module CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           Abstract.AdhesiveHLR                  (EpiPairs)
import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.EssentialCriticalPairs
import           Analysis.Interlevel.EvolutionarySpans
import           Analysis.Interlevel.InterLevelCP
import           Control.Monad                         (when)
import           Data.List.Utils
import           Data.Matrix                           hiding ((<|>))
import           Data.Monoid                           ((<>))
import qualified Data.Set                              as Set
import           GlobalOptions
import           Options.Applicative
import           SndOrder.Rule
import qualified XML.GGXReader                         as XML
import qualified XML.GGXWriter                         as GW

data Options = Options
  { outputFile    :: Maybe String
  , sndOrder      :: Bool
  , essentialFlag :: Bool
  , analysisType  :: AnalysisType
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
  <*> essentialCP
  <*> cpAnalysisType

cpOrder :: Parser Bool
cpOrder = flag False True
    ( long "snd-order"
    <> help "Set the analysis to the second order rules")

essentialCP :: Parser Bool
essentialCP = flag False True
    ( long "essential"
    <> help "Compute the Essential Critical Pairs analysis (Warning: not fully supported yet)")

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
    let dpoConf = morphismsConf globalOpts

    (fstOrderGG, sndOrderGG, printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let action = analysisType opts
        essentialCP = essentialFlag opts
        secondOrder = sndOrder opts
        writer = defWriterFun essentialCP secondOrder dpoConf action

        namedFstOrdRules = productions fstOrderGG
        namedSndOrdRules = productions sndOrderGG
        fstOrdRules = map snd namedFstOrdRules
        sndOrdRules = map snd namedSndOrdRules

        interlevelCPs = applySecondOrder (interLevelCP dpoConf) namedFstOrdRules namedSndOrdRules
        interlevelWithoutCounting = Set.fromList $ map (\(x,y,_,_) -> (x,y)) interlevelCPs
        evoConflicts = allEvolSpans dpoConf namedSndOrdRules


    putStrLn $ "injective satisfiability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""

    when secondOrder $ mapM_ putStrLn (XML.printMinimalSafetyNacsLog printNewNacs)

    when essentialCP $ putStrLn "Warning: essential critical pairs not fully supported"
    putStrLn ""

    let fstOrderAnalysis = printAnalysis essentialCP action dpoConf fstOrdRules
        sndOrderAnalysis = printAnalysis essentialCP action dpoConf sndOrdRules
    case outputFile opts of
      Just file ->
        do
          putStrLn "Warning: exporting conflicts/dependencies to .cpx not fully supported."
          writer (fstOrderGG, sndOrderGG) ggName names file
      Nothing ->
        if secondOrder
          then sndOrderAnalysis
          else fstOrderAnalysis

    when secondOrder $
      mapM_ putStrLn $
        "Inter-level Critical Pairs Analysis" :
        "This log shows a list of (first-order rule, second-order rule) that are in conflict:" :
        [show interlevelWithoutCounting] --map printILCP interlevelCPs

    putStrLn ""

    when secondOrder $
      mapM_ putStrLn $
        "Evolutionary Spans Inter-level CP:" :
        "This log shows pairs of (second-order rule, second-order rule, number of FolFol, number of ConfFol, number of FolConf, number of ConfConf)" :
        printEvoConflicts evoConflicts

    putStrLn ""
    putStrLn "Critical Pair Analysis done!"

-- | Inter-level CP to Strings
--printILCP :: (String, String, Int, InterLevelCP a b) -> String
--printILCP (fstName, sndName, idx, _) =
--  fstName ++ " " ++ sndName ++ " (id:" ++ show idx ++ ") ... (conflict omitted)"

-- | Evolutionary Spans to Strings
printEvoConflicts :: [(String, String, [EvoSpan a b])] -> [String]
printEvoConflicts = map printOneEvo
  where
    fst (y,_,_) = y
    snd (_,y,_) = y
    thd (_,_,y) = y

    printOneEvo e = "(" ++ fst e ++ ", " ++ snd e ++ ", " ++
                       show (printConf (False,False) (thd e)) ++ ", " ++
                       show (printConf (True,False) (thd e)) ++ ", " ++
                       show (printConf (False,True) (thd e)) ++ ", " ++
                       show (printConf (True,True) (thd e)) ++ ")"
    printConf str evos = countElem str (map cpe evos)

printAnalysis :: (EpiPairs m, DPO m) =>
  Bool -> AnalysisType -> MorphismsConfig -> [Production m] -> IO ()
printAnalysis essential action dpoConf rules =
  let essentialConfMatrix = analysisMatrix dpoConf rules
        findAllEssentialDeleteUse findAllEssentialProduceDangling findAllEssentialProduceForbid
        "Essential Delete-Use" "Essential Produce-Dangling" "Essential Produce-Forbid" "Essential Conflicts"
      confMatrix = analysisMatrix dpoConf rules
        findAllDeleteUse findAllProduceDangling findAllProduceForbid
        "Delete-Use" "Produce-Dangling" "Produce-Forbid" "Conflicts"
      depMatrix = triDepMatrix ++ irrDepMatrix
      triDepMatrix = analysisMatrix dpoConf rules
        findAllProduceUse findAllRemoveDangling findAllDeleteForbid
        "Produce-Use" "Remove-Dangling" "Deliver-Forbid" "Triggered Dependencies"
      irrDepMatrix = analysisMatrix dpoConf rules
        findAllDeliverDelete findAllDeliverDangling findAllForbidProduce
        "Deliver-Delete" "Deliver-Dangling" "Forbid-Produce" "Irreversible Dependencies"
  in mapM_
       putStrLn $
       (case (essential, calculateConflicts action) of
         (True, True)  -> essentialConfMatrix
         (False, True) -> confMatrix
         _             -> []
       )
       ++ (if calculateDependencies action then depMatrix else [])

-- Receives functions and theirs names,
-- and returns they applicated to the rules
analysisMatrix :: MorphismsConfig -> [Production m]
  -> (MorphismsConfig -> Production m -> Production m -> [cps])
  -> (MorphismsConfig -> Production m -> Production m -> [cps])
  -> (MorphismsConfig -> Production m -> Production m -> [cps])
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

defWriterFun :: Bool -> Bool -> MorphismsConfig -> AnalysisType
             -> GW.Grammars a b -> String
             -> [(String,String)] -> String -> IO ()
defWriterFun essential secondOrder conf t =
  case (secondOrder,t) of
    (False, Conflicts)    -> GW.writeConflictsFile essential conf
    (False, Dependencies) -> GW.writeDependenciesFile conf
    (False, Both)         -> GW.writeConfDepFile essential conf
    (True, Conflicts)     -> GW.writeSndOderConflictsFile conf
    (True, Dependencies)  -> GW.writeSndOderDependenciesFile conf
    (True, Both)          -> GW.writeSndOderConfDepFile conf
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
