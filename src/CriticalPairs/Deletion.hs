module CriticalPairs.Deletion (
   setDeletion,
   ndelByRight,
   ndelByLeft,
   edelByLeft,
   edelByRight
   ) where

import Graph.GraphRule
import Graph.Graph
import qualified CriticalPairs.GraphPart as GP
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Abstract.Relation as R

setDeletion :: GraphRule a b -> GraphRule a b -> [GP.EqClassGraphMap] -> [GP.EqClassGraphMap]
setDeletion l r [] = []
setDeletion l r ((GP.EqClassGraphMap g nM eM):xs) = (GP.EqClassGraphMap (setDel l r g) nM eM):(setDeletion l r xs)

{-Funções que verificam se nodos e arestas serão deletados-}
setDel :: GraphRule a b -> GraphRule a b -> GP.EqClassGraph -> GP.EqClassGraph
setDel l r g@(nodes,edges) = (setDelNodes l r nodes, setDelEdges l r edges)

setDelNodes :: GraphRule a b -> GraphRule a b -> [[GP.Node]] -> [[GP.Node]]
setDelNodes l r []   = []
setDelNodes l r [[]] = []
setDelNodes l r (x:xs) = (map (verifyDelNodes l r) x):(setDelNodes l r xs)

verifyDelNodes :: GraphRule a b -> GraphRule a b -> GP.Node -> GP.Node
verifyDelNodes l r n@(GP.Node a1 a2 a3 _) = GP.Node a1 a2 a3 (case (willBeDelNode l "Left" n, willBeDelNode r "Right" n) of
                (True,True)   -> GP.Both
                (True,False)  -> GP.Left
                (False,True)  -> GP.Right
                (False,False) -> GP.Not)

setDelEdges :: GraphRule a b -> GraphRule a b -> [[GP.Edge]] -> [[GP.Edge]]
setDelEdges l r []   = []
setDelEdges l r [[]] = []
setDelEdges l r (x:xs) = (map (verifyDelEdges l r) x):(setDelEdges l r xs)

verifyDelEdges :: GraphRule a b -> GraphRule a b -> GP.Edge -> GP.Edge
verifyDelEdges l r n@(GP.Edge a1 a2 a3 a4 a5 _) = GP.Edge a1 a2 a3 a4 a5 (case (willBeDelEdge l "Left" n, willBeDelEdge r "Right" n) of
                (True,True)   -> GP.Both
                (True,False)  -> GP.Left
                (False,True)  -> GP.Right
                (False,False) -> GP.Not)

ndelByRight = ndelBy "Right" 
ndelByLeft  = ndelBy "Left" 

ndelBy :: String -> GP.Node -> Bool
ndelBy "Left"  n = ((GP.ndel n) == GP.Left)  || ((GP.ndel n) == GP.Both)
ndelBy "Right" n = ((GP.ndel n) == GP.Right) || ((GP.ndel n) == GP.Both)

edelByLeft  = edelBy "Left"
edelByRight = edelBy "Right"

edelBy :: String -> GP.Edge -> Bool
edelBy "Left"  n = ((GP.edel n) == GP.Left)  || ((GP.edel n) == GP.Both)
edelBy "Right" n = ((GP.edel n) == GP.Right) || ((GP.edel n) == GP.Both)

willBeDelEdge :: GraphRule a b -> String -> GP.Edge -> Bool
willBeDelEdge rule s n = edgeOfTheRule && edgeInCodomain && applyIsNull
   where
      edgeOfTheRule  = s == (GP.egsource n)
      edgeInCodomain = edgeid `elem` (R.domain $ R.inverse $ GM.edgeRelation $ TGM.mapping (left rule))
      applyIsNull    = [] == (R.apply (R.inverse $ GM.edgeRelation $ TGM.mapping (left rule)) edgeid)
      edgeid         = EdgeId (GP.label n)

willBeDelNode :: GraphRule a b -> String -> GP.Node -> Bool
willBeDelNode rule s n = nodeOfTheRule && nodeInCodomain && applyIsNull
   where
      nodeOfTheRule  = s == (GP.ngsource n)
      nodeInCodomain = nodeid `elem` (R.domain $ R.inverse $ GM.nodeRelation $ TGM.mapping (left rule))
      applyIsNull    = [] == (R.apply (R.inverse $ GM.nodeRelation $ TGM.mapping (left rule)) nodeid)
      nodeid         = NodeId (GP.nname n)
