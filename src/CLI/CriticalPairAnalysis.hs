module CLI.CriticalPairAnalysis
  ( Options
  , options
  , execute
  ) where

import           Abstract.DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.InterLevelCP
import           CLI.GlobalOptions
import           Data.List
import           Data.List.Split
import           Data.List.Utils
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

    let nacInj = injectiveNacSatisfaction globalOpts
        onlyInj = arbitraryMatches globalOpts
        action = analysisType opts
        secondOrder = sndOrder opts
        writer = defWriterFun secondOrder nacInj onlyInj action
        -- First order conflicts/dependencies
        rules = map snd (GG.rules gg)
        puMatrix = pairwiseCompare (allProduceUse nacInj onlyInj) rules
        rdMatrix = pairwiseCompare (allRemoveDangling nacInj onlyInj) rules
        ddMatrix = pairwiseCompare (allDeliverDelete nacInj onlyInj) rules
        udMatrix = pairwiseCompare (allDeleteUse nacInj onlyInj) rules
        peMatrix = pairwiseCompare (allProduceDangling nacInj onlyInj) rules
        pfMatrix = pairwiseCompare (allProduceForbid nacInj onlyInj) rules
        conflictsMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) udMatrix pfMatrix peMatrix
        dependenciesMatrix = liftMatrix3 (\x y z -> x ++ y ++ z) puMatrix rdMatrix ddMatrix

        -- Inter Level conflicts
        conf = applySecondOrder (interLevelConflict nacInj onlyInj) (GG.rules gg) (GG.sndOrderRules gg)
        f str = join "_" (take 2 (splitOn "_" str))
        printILCP = "Interlevel Critical Pairs" :
                    "2rule_rule (number of conflicts)" :
                    map (\x -> f (head x) ++ " " ++ show (length x))
                        (groupBy (\x y -> f x == f y) (map fst conf))

        evoConflicts = map (\r1 -> map (evo nacInj onlyInj r1) (GG.sndOrderRules gg)) (GG.sndOrderRules gg)

        -- Second order conflicts/dependencies
        newNacs =
          map (\(n,r) ->
            let newRule = addMinimalSafetyNacs nacInj r
                tamNewNacs = length (nacs newRule)
                tamNacs = length (nacs r)
             in ((n, newRule), (n, tamNewNacs - tamNacs))
             ) (GG.sndOrderRules gg)
        rules2 = map (snd . fst) newNacs
        newGG = gg {GG.sndOrderRules = map fst newNacs}

        printNewNacs = map snd newNacs

        ud2Matrix = pairwiseCompare (allDeleteUse nacInj onlyInj) rules2
        pd2Matrix = pairwiseCompare (allProduceDangling nacInj onlyInj) rules2
        pf2Matrix = pairwiseCompare (allProduceForbid nacInj onlyInj) rules2
        pu2Matrix = pairwiseCompare (allProduceUse nacInj onlyInj) rules2
        rd2Matrix = pairwiseCompare (allRemoveDangling nacInj onlyInj) rules2
        dd2Matrix = pairwiseCompare (allDeliverDelete nacInj onlyInj) rules2
        conflicts2Matrix = liftMatrix3 (\x y z -> x ++ y ++ z) ud2Matrix pd2Matrix pf2Matrix
        dependencies2Matrix = liftMatrix3 (\x y z -> x ++ y ++ z) pu2Matrix rd2Matrix dd2Matrix

        conflicts = [ "Delete-Use:"
                , show (length <$> udMatrix)
                , ""
                , "Produce-Dangling:"
                , show (length <$> peMatrix)
                , ""
                , "Produce-Forbid:"
                , show (length <$> pfMatrix)
                , ""
                , "All Conflicts:"
                , show (length <$> conflictsMatrix)
                , ""]

        dependencies = [ "Produce-Use Dependency:"
                   , show (length <$> puMatrix)
                   , ""
                   , "Remove-Dangling Dependency:"
                   , show (length <$> rdMatrix)
                   , ""
                   , "Deliver-Delete Dependency:"
                   , show (length <$> ddMatrix)
                   , ""
                   , "All Dependencies:"
                   , show (length <$> dependenciesMatrix)
                   , ""]

        conflicts2 = [ "Second Order Delete-Use:"
                , show (length <$> ud2Matrix)
                , ""
                , "Second Order Produce-Dangling:"
                , show (length <$> pd2Matrix)
                , ""
                , "Second Order Produce-Forbid:"
                , show (length <$> pf2Matrix)
                , ""
                , "All Second Order Conflicts:"
                , show (length <$> conflicts2Matrix)
                , ""]

        dependencies2 = [ "Second Order Produce Use Dependency:"
                   , show (length <$> pu2Matrix)
                   , ""
                   , "Second Order Remove-Dangling Dependency:"
                   , show (length <$> rd2Matrix)
                   , ""
                   , "Second Order Deliver Delete Dependency:"
                   , show (length <$> dd2Matrix)
                   , ""
                   , "All Second Order Dependencies:"
                   , show (length <$> dependencies2Matrix)
                   , ""]

    putStrLn $ "injective satisfability of nacs: " ++ show nacInj
    putStrLn $ "only injective matches morphisms: " ++ show onlyInj
    putStrLn ""

    if secondOrder
      then mapM_
        putStrLn $
        ["Adding minimal safety nacs to second order rules:"]
        ++ (if onlyInj == MonoMatches then [] else ["Warning, some nacs for non injective matches are not implemented"])
        ++ map (\(r,n) -> "Rule " ++ r ++ ", added " ++ show n ++ " nacs") printNewNacs
        ++ ["All minimal safety nacs added!", ""]
      else
        putStrLn ""

    case outputFile opts of
      Just file -> writer newGG ggName names file
      Nothing -> let (confMatrix, depMatrix) =
                       if secondOrder
                         then (conflicts2, dependencies2)
                         else (conflicts, dependencies)
                 in
                   mapM_
                   putStrLn $
                   (if calculateConflicts action then confMatrix else [])
                   ++ (if calculateDependencies action then depMatrix else [])
                   ++ ["Done!"]

    case (secondOrder, onlyInj) of
      (True, AnyMatches) -> mapM_ putStrLn printILCP
      (True, MonoMatches) -> putStrLn "Interlevel CP not defined for only injective matches"
      _ -> mapM_ putStrLn []

    putStrLn "Evolution Interlevel CP"
    print evoConflicts

defWriterFun :: Bool -> NacSatisfaction -> MatchRestriction -> AnalysisType
             ->(GG.GraphGrammar a b -> String
             -> [(String,String)] -> String -> IO ())
defWriterFun secondOrder nacInj inj t =
  case (secondOrder,t) of
    (False, Conflicts)    -> GW.writeConflictsFile nacInj inj
    (False, Dependencies) -> GW.writeDependenciesFile nacInj inj
    (False, Both)         -> GW.writeConfDepFile nacInj inj
    (True, Conflicts)     -> GW.writeSndOderConflictsFile nacInj inj
    (True, Dependencies)  -> GW.writeSndOderDependenciesFile nacInj inj
    (True, Both)          -> GW.writeSndOderConfDepFile nacInj inj
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
