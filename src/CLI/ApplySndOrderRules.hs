{-# LANGUAGE TypeFamilies #-}

module ApplySndOrderRules
  ( Options
  , options
  , execute
  ) where

import           Abstract.AdhesiveHLR
import qualified Data.Set as S
import           Graph.Graph
import           GlobalOptions
import           Options.Applicative
import qualified SndOrder.Rule           as SO
import qualified TypedGraph.GraphGrammar as GG
import           TypedGraph.Morphism.Cocomplete
import qualified TypedGraph.GraphRule    as GR
import qualified XML.GGXReader           as XML
import qualified XML.GGXWriter           as GW

data Options = Options
  { outputFile :: String }

options :: Parser Options
options = Options
  <$> strOption
    ( long "output-file"
    <> short 'o'
    <> metavar "FILE"
    <> action "file"
    <> help "GGX file that will be written, adding the new rules to the original graph grammar")

addEmptyFstOrderRule :: Graph a b -> [(String,GR.GraphRule a b)] -> [(String,GR.GraphRule a b)]
addEmptyFstOrderRule typegraph fstRules =
  if any (== True) (map (GR.nullGraphRule . snd) fstRules) then
    fstRules
  else
    fstRulesPlusEmpty

  where
    fstRulesPlusEmpty = ("emptyRule", emptyFstOrderRule) : fstRules
    emptyFstOrderRule = GR.emptyGraphRule typegraph

execute :: GlobalOptions -> Options -> IO ()
execute globalOpts opts = do
    let dpoConf = dpoConfig globalOpts

    (gg,printNewNacs) <- XML.readGrammar (inputFile globalOpts) (useConstraints globalOpts) dpoConf
    ggName <- XML.readGGName (inputFile globalOpts)
    names <- XML.readNames (inputFile globalOpts)

    putStrLn "Reading the second order graph grammar..."
    putStrLn ""

    putStrLn $ "injective satisfability of nacs: " ++ show (nacSatisfaction dpoConf)
    putStrLn $ "only injective matches morphisms: " ++ show (matchRestriction dpoConf)
    putStrLn ""

    --mapM_ putStrLn (XML.printMinimalSafetyNacsLog printNewNacs)

    -- It is adding an empty first order rule as possible match target,
    -- it allows the creation from "zero" of a new second order rules.
    let fstRulesPlusEmpty = addEmptyFstOrderRule (GG.typeGraph gg) (GG.rules gg)
        newRules = SO.applySecondOrder (SO.applySndOrderRule dpoConf) fstRulesPlusEmpty (GG.sndOrderRules gg)
        gg2 = gg {GG.rules = GG.rules gg ++ newRules}
    
    putStrLn ""

    let n1 = (NodeId 1, NodeId 10)
        n2 = (NodeId 2, NodeId 10)
        n3 = (NodeId 3, NodeId 10)
        n4 = (NodeId 4, NodeId 10)
        n5 = (NodeId 5, NodeId 10)
        p1 = S.fromList [(n2,n3),(n3,n4),(n1,n5)]
        p2 = S.fromList [S.fromList[n1],S.fromList[n2],S.fromList[n3],S.fromList[n4],S.fromList[n5]]
        p3 = S.fromList [[n1,n2],[n3]]
        restSet set = S.difference set (S.singleton $ S.elemAt 0 set)

    print p1
    print p2
    print $ construct p1 p2
    --print $ mergeEquivalences (n2,n3) p2

    --GW.writeGrammarFile gg2 ggName names (outputFile opts)

    putStrLn "Done!"
    putStrLn ""
