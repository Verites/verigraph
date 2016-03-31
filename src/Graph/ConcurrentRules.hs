module Graph.ConcurrentRules
( allConcurrentRules,
  maxConcurrentRule
) where

import qualified Abstract.Morphism as M
import qualified CriticalPairs.CriticalPairs as CP
import           Data.List
import           Graph.GraphRule
import           Graph.NacOperations
import qualified Graph.TypedGraphMorphism as TGM
import qualified Graph.Rewriting as R

type EpiPair a b = (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)

allConcurrentRules :: [GraphRule a b] -> [GraphRule a b]
allConcurrentRules [] = []
allConcurrentRules (x:[]) = [x]
allConcurrentRules (x:xs) = concat $ (map (concurrentRules x)) $ allConcurrentRules xs

maxConcurrentRule :: [GraphRule a b] -> GraphRule a b
maxConcurrentRule rules = head $ maxConcurrentRules rules

maxConcurrentRules :: [GraphRule a b] -> [GraphRule a b]
maxConcurrentRules [] = []
maxConcurrentRules (x:[]) = [x]
maxConcurrentRules (x:xs) = map (maxConcurrentRuleForLastPair x) (maxConcurrentRules xs)

concurrentRules :: GraphRule a b -> GraphRule a b -> [GraphRule a b]
concurrentRules c n = map (concurrentRuleForPair c n) $ pairs c n

pairs :: GraphRule a b -> GraphRule a b -> [EpiPair a b]
pairs c n = CP.createPairs (right c) (left n)

maxConcurrentRuleForLastPair :: GraphRule a b -> GraphRule a b -> GraphRule a b
maxConcurrentRuleForLastPair c n = concurrentRuleForPair c n (last $ pairs c n)

concurrentRuleForPair :: GraphRule a b -> GraphRule a b -> EpiPair a b -> GraphRule a b
concurrentRuleForPair c n pair = graphRule l r (dmc ++ lp)
  where
    pocC = R.poc (fst pair) (right c)
    pocN = R.poc (snd pair) (left n)
    poC = R.po (fst pocC) (left c)
    poN = R.po (fst pocN) (right n)
    pb = injectivePullback (snd pocC) (snd pocN)
    l = M.compose (fst pb) (snd poC)
    r = M.compose (snd pb) (snd poN)
    dmc = concat $ map (downwardShift (fst poC)) (nacs c)
    p = graphRule (snd poC) (snd pocC) []
    den = concat $ map (downwardShift (snd pair)) (nacs n)
    lp = concat $ map (leftShiftNac p) den

injectivePullback :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
injectivePullback f g = (delNodesFromF', delNodesFromG')
  where
    f' = TGM.invertTGM f
    g' = TGM.invertTGM g
    nodes = TGM.nodesDomain f'
    edges = TGM.edgesDomain f'
    knodes = filter (\n -> TGM.applyNodeTGM f' n /= Nothing && TGM.applyNodeTGM g' n /= Nothing) nodes
    kedges = filter (\e -> TGM.applyEdgeTGM f' e /= Nothing && TGM.applyEdgeTGM g' e /= Nothing) edges
    delNodes = nodes \\ knodes
    delEdges = edges \\ kedges
    delEdgesFromF' = foldr TGM.removeEdgeDomTyped f' delEdges
    delNodesFromF' = foldr TGM.removeNodeDomTyped delEdgesFromF' delNodes
    delEdgesFromG' = foldr TGM.removeEdgeDomTyped g' delEdges
    delNodesFromG' = foldr TGM.removeNodeDomTyped delEdgesFromG' delNodes

