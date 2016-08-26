module CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           Abstract.AdhesiveHLR      (EpiPairs)
import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
--import           Analysis.InterLevelCP
import           GlobalOptions
--import           Data.List
--import           Data.List.Split
--import           Data.List.Utils
import           Data.Matrix               hiding ((<|>))
import qualified TypedGraph.GraphGrammar        as GG
import           SndOrder.Rule
import           Options.Applicative
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
    gg <- XML.readGrammar (inputFile globalOpts)
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Analyzing the graph grammar..."
    putStrLn ""

    let dpoConf = dpoConfig globalOpts
        action = analysisType opts
        secondOrder = sndOrder opts
        writer = defWriterFun secondOrder dpoConf action
        -- First order conflicts/dependencies
        rules = map snd (GG.rules gg)

        -- Inter Level conflicts
        --conf = applySecondOrder (interLevelConflict nacInj onlyInj) (GG.rules gg) (GG.sndOrderRules gg)
        --f str = join "_" (take 2 (splitOn "_" str))
        --printILCP = "Interlevel Critical Pairs" :
        --            "2rule_rule (number of conflicts)" :
        --            map (\x -> f (head x) ++ " " ++ show (length x))
        --                (groupBy (\x y -> f x == f y) (map fst conf))

        --evoConflicts = map (\r1 -> map (evo nacInj onlyInj r1) (GG.sndOrderRules gg)) (GG.sndOrderRules gg)

        -- Second order conflicts/dependencies
        newNacs =
          map (\(n,r) ->
            let newRule = addMinimalSafetyNacs dpoConf r
                tamNewNacs = length (getNACs newRule)
                tamNacs = length (getNACs r)
             in ((n, newRule), (n, tamNewNacs - tamNacs))
             ) (GG.sndOrderRules gg)
        rules2 = map (snd . fst) newNacs
        newGG = gg {GG.sndOrderRules = map fst newNacs}

        printNewNacs = map snd newNacs

    putStrLn $ "injective satisfability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""

    if secondOrder
      then mapM_
        putStrLn $
        ["Adding minimal safety nacs to second order rules:"]
        ++ (if matchRestriction dpoConf == MonoMatches then [] else ["Warning, some nacs for non injective matches are not implemented"])
        ++ map (\(r,n) -> "Rule " ++ r ++ ", added " ++ show n ++ " nacs") printNewNacs
        ++ ["All minimal safety nacs added!", ""]
      else
        putStrLn ""

    let fstOrderAnalysis = printAnalysis action dpoConf rules
        sndOrderAnalysis = printAnalysis action dpoConf rules2
    case outputFile opts of
      Just file -> writer newGG ggName names file
      Nothing -> if secondOrder
                   then sndOrderAnalysis
                   else fstOrderAnalysis

    --case (secondOrder, matchRestriction dpoConf) of
    --  (True, AnyMatches) -> mapM_ putStrLn printILCP
    --  (True, MonoMatches) -> putStrLn "Interlevel CP not defined for only injective matches"
    --  _ -> mapM_ putStrLn []

    --putStrLn "Evolution Interlevel CP"
    --print evoConflicts

printAnalysis :: (EpiPairs m, DPO m) =>
  AnalysisType -> DPOConfig -> [Production m] -> IO ()
printAnalysis action dpoConf rules =
  let confMatrix = analysisMatrix dpoConf rules
        allDeleteUse allProduceDangling allProduceForbid
        "Delete-Use" "Produce-Dangling" "Produce-Forbid" "Conflicts"
      depMatrix = triDepMatrix ++ irrDepMatrix
      triDepMatrix = analysisMatrix dpoConf rules
        allProduceUse allRemoveDangling allDeleteForbid
        "Produce-Use" "Remove-Dangling" "Deliver-Forbid" "Triggereds Dependencies"
      irrDepMatrix = analysisMatrix dpoConf rules
        allDeliverDelete allDeliverDangling allForbidProduce
        "DeliverDelete" "Deliver-Dangling" "Forbid-Produce" "Irreversibles Dependencies"
  in mapM_
       putStrLn $
       (if calculateConflicts action then confMatrix else [])
       ++ (if calculateDependencies action then depMatrix else [])
       ++ ["Done!"]

-- Receives functions that theirs names,
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
