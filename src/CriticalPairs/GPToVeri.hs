module CriticalPairs.GPToVeri (
   setType,
   mountTGM,
   mountTGMBoth,
   toMorphism
   ) where

import Graph.GraphRule
import qualified CriticalPairs.GraphPart as GP
import qualified Graph.GraphMorphism as GM
import qualified Graph.TypedGraphMorphism as TGM
import qualified Abstract.Morphism as M
import Graph.Graph
import Data.Maybe (fromJust)

{-Conversão para as estruturas do Verigraph-}

mountTGMBoth :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
             -> GP.EqClassGraphMap
             -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
mountTGMBoth l r g = (mountTGM l "Left" g, mountTGM r "Right" g)

mountTGM :: TGM.TypedGraphMorphism a b -> String -> GP.EqClassGraphMap -> TGM.TypedGraphMorphism a b
mountTGM morph side g = TGM.typedMorphism (M.codomain morph) typeMorphism morphism
    where
        m = TGM.typedMorphism (M.codomain morph) typeMorphism morphism
        typeMorphism = setType g (GM.empty (M.codomain morphism) (M.codomain $ M.domain morph))
        morphism = toMorphism g side

{-Cria o morfismo que tipa o grafo G criado-}
setType :: GP.EqClassGraphMap -> GM.GraphMorphism a b -> GM.GraphMorphism a b
setType g@(GP.EqClassGraphMap (nodes,edges) nM eM) m = setTypeEdges g edges (setTypeNodes g nodes m)

setTypeNodes :: GP.EqClassGraphMap -> [[GP.Node]] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
setTypeNodes g   []   m = m
setTypeNodes g  [[]]  m = m
setTypeNodes g (n:ns) m = setTypeNodes g ns (GM.updateNodes nodeid typeid m)
  where
    nodeid = NodeId $ fromJust $ lookup n (GP.nodeMap g)
    typeid = NodeId $ GP.ntype (head n)

setTypeEdges :: GP.EqClassGraphMap -> [[GP.Edge]] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
setTypeEdges g   []   m = m
setTypeEdges g  [[]]  m = m
setTypeEdges g (e:es) m = setTypeEdges g es (GM.updateEdges edgeid typeid m)
  where
    edgeid = EdgeId $ fromJust $ lookup e (GP.edgeMap g)
    typeid = EdgeId $ GP.etype (head e)

{-Cria o morfismo do lado esquerdo da regra para o grafo G criado-}
toMorphism :: GP.EqClassGraphMap -> String -> GM.GraphMorphism a b
toMorphism g@(GP.EqClassGraphMap (nodes,edges) nM eM) s = setEdges
  where
     setNodes = toMorphismNodes s g nodes (GM.empty (Graph.Graph.empty) (toGraph g))
     setEdges = toMorphismEdges s g edges setNodes

toGraph :: GP.EqClassGraphMap -> Graph a b
toGraph g@(GP.EqClassGraphMap ([],[]) nM eM) = Graph.Graph.empty
toGraph g@(GP.EqClassGraphMap (nodes,[]) nM eM) = insertNode newNode newG
    where
        newNode = NodeId (fromJust $ lookup (head nodes) nM)
        newG = toGraph (GP.EqClassGraphMap (tail nodes,[]) nM eM)
toGraph g@(GP.EqClassGraphMap (nodes,edges) nM eM) = insertEdge newEdge nodeSrc nodeTgt newG
    where
        newEdge = EdgeId (fromJust $ lookup (head edges) eM)
        nodeSrc = NodeId (fromJust $ lookup (GP.getListNode nodes (GP.source $ head $ head edges)) nM)
        nodeTgt = NodeId (fromJust $ lookup (GP.getListNode nodes (GP.target $ head $ head edges)) nM)
        newG = toGraph (GP.EqClassGraphMap (nodes,tail edges) nM eM)

toMorphismNodes :: String -> GP.EqClassGraphMap -> [[GP.Node]] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
toMorphismNodes s g   []   m = m
toMorphismNodes s g  [[]]  m = m
toMorphismNodes s g (x:xs) m = toMorphismNodes s g xs (addNodes s eqnodeid x m)
  where
    eqnodeid      = NodeId $ fromJust $ lookup x (GP.nodeMap g)

addNodes :: String -> NodeId -> [GP.Node] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
addNodes s eqnodeid []     m = m
addNodes s eqnodeid (x:xs) m = (addNodes s eqnodeid xs) (if sideEqualNodeSide then newGM else m)
  where
    sideEqualNodeSide = s == (GP.ngsource x)
    nodeid            = NodeId (GP.nname x)
    newDomain         = insertNode nodeid (M.domain m)
    newDomainGM       = GM.updateDomain newDomain m
    newGM             = GM.updateNodes nodeid eqnodeid newDomainGM

toMorphismEdges :: String -> GP.EqClassGraphMap -> [[GP.Edge]] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
toMorphismEdges s g   []   m = m
toMorphismEdges s g  [[]]  m = m
toMorphismEdges s g (x:xs) m = toMorphismEdges s g xs (addEdges s eqedgeid g x m)
  where
    eqedgeid      = EdgeId $ fromJust $ lookup x (GP.edgeMap g)
    nodeIdSrcF    = NodeId $ fromJust $ lookup (GP.getListNode (fst $ GP.eqGraph g) (GP.source (head x))) (GP.nodeMap g)
    nodeIdTgtF    = NodeId $ fromJust $ lookup (GP.getListNode (fst $ GP.eqGraph g) (GP.target (head x))) (GP.nodeMap g)

addEdges :: String -> EdgeId -> GP.EqClassGraphMap -> [GP.Edge] -> GM.GraphMorphism a b -> GM.GraphMorphism a b
addEdges s eqedgeid g []     m = m
addEdges s eqedgeid g (x:xs) m = if sideEqualEdgeSide then addEdges s eqedgeid g xs newGM else addEdges s eqedgeid g xs m
  where
    sideEqualEdgeSide = s == (GP.egsource x)
    edgeid            = EdgeId (GP.label x)
    nodeIdSrc         = NodeId (GP.nname (GP.source x))
    nodeIdTgt         = NodeId (GP.nname (GP.target x))
    newDomain         = insertEdge edgeid nodeIdSrc nodeIdTgt (M.domain m)
    newDomainGM       = GM.updateDomain newDomain m
    newGM             = GM.updateEdges edgeid eqedgeid newDomainGM
