module Main (main) where

import           Data.Matrix                           hiding ((<|>))
import           Data.Monoid                           ((<>))
import           Options.Applicative

import           Abstract.Category.FinitaryCategory
import           Abstract.Category.JointlyEpimorphisms
import           Abstract.Rewriting.DPO                as DPO
import           Analysis.CriticalPairs
import           Analysis.CriticalSequence
import           Analysis.EssentialCriticalPairs
import           GlobalOptions
import           Rewriting.DPO.TypedGraph
import qualified XML.GPRReader.GXLReader               as GPR
import qualified XML.GGXWriter                         as GW

data AnalysisType = Conflicts | Dependencies | None deriving (Eq)

main :: IO ()
main =
  execParser opts >>= execute

  where
    opts =
      info (helper <*> allOptions)
           ( fullDesc
           <> progDesc "Loads the given graph grammar in the Groove Productions System (.gps).")


data Options = Options
  { outputFile        :: Maybe String
  , criticalPairs     :: Bool
  , criticalSequences :: Bool
  , essentialFlag     :: Bool
  }

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
    ( long "cpa"
    <> help "Runs the Critical Pairs analysis")
  <*> flag False True
    ( long "csa"
    <> help "Runs the Critical Sequences analysis")
  <*> flag False True
    ( long "essential"
    <> help "Compute the Essential Critical Pairs analysis (Warning: not fully supported yet)")

allOptions :: Parser (GlobalOptions, Options)
allOptions =
  (,)
  <$> globalOpts
  <*> options

execute :: (GlobalOptions, Options) -> IO ()
execute (globalOpts, options) =
  do
    (fstOrderGrammar,names) <- GPR.readGrammar (inputFile globalOpts)
    
    putStrLn "Loading the graph grammar..."
    putStrLn ""
    
    let ggName = GPR.readGGName (inputFile globalOpts)
        dpoConf = morphismsConf globalOpts
        action
          | criticalPairs options     = Conflicts
          | criticalSequences options = Dependencies
          | otherwise                 = None
        
        essentialCP = essentialFlag options
        writer = defWriterFun essentialCP dpoConf action
        
        namedRules = DPO.productions fstOrderGrammar
        rules = map snd namedRules
        
        -- creates an empty second-order grammar for the writer function
        typeGraph = codomain (codomain (getLHS (head rules)))
        emptySndOrderGrammar = grammar (emptyGraphRule typeGraph) [] []
        
    let analysis = printAnalysis essentialCP action dpoConf rules
    case outputFile options of
      Just file ->
        do
          putStrLn "Warning: exporting conflicts/dependencies to .cpx not fully supported."
          writer (fstOrderGrammar,emptySndOrderGrammar) ggName names file
      Nothing -> analysis
    
    putStrLn ""
    putStrLn "Bye!"
    
    return ()

printAnalysis :: (JointlyEpimorphisms morph, DPO morph) =>
  Bool -> AnalysisType -> MorphismsConfig -> [Production morph] -> IO ()
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
       putStrLn
       (case (essential, action) of
         (True , Conflicts) -> essentialConfMatrix
         (False, Conflicts) -> confMatrix
         (_,  Dependencies) -> depMatrix
         _             -> []
       )

-- Receives functions and theirs names,
-- and returns they applicated to the rules
analysisMatrix :: MorphismsConfig -> [Production morph]
  -> (MorphismsConfig -> Production morph -> Production morph -> [cps])
  -> (MorphismsConfig -> Production morph -> Production morph -> [cps])
  -> (MorphismsConfig -> Production morph -> Production morph -> [cps])
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

defWriterFun :: Bool -> MorphismsConfig -> AnalysisType
             -> GW.Grammars a b -> String
             -> [(String,String)] -> String -> IO ()
defWriterFun essential conf t =
  case t of
    Conflicts    -> GW.writeConflictsFile essential conf
    Dependencies -> GW.writeDependenciesFile conf
    None         -> GW.writeGrammarFile

-- | Combine three matrices with the given function. All matrices _must_ have
-- the same dimensions.
liftMatrix3 :: (a -> b -> c -> d) -> Matrix a -> Matrix b -> Matrix c -> Matrix d
liftMatrix3 f ma mb mc = matrix (nrows ma) (ncols ma) $ \pos ->
  f (ma!pos) (mb!pos) (mc!pos)

pairwiseCompare :: (a -> a -> b) -> [a] -> Matrix b
pairwiseCompare compare items =
  matrix (length items) (length items) $ \(i,j) ->
    compare (items !! (i-1)) (items !! (j-1))
