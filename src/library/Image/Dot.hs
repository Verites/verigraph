module Image.Dot where

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO.StateSpace
import qualified Category.TypedGraph as TGraph
import Category.TypedGraphRule (TypedGraphRule, Production(..), mappingLeft, mappingRight, mappingInterface)
import           Data.Graphs                        hiding (Node (..))
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraphRule


import qualified Data.IntMap                        as IntMap
import qualified Data.Set                           as Set
import           Text.PrettyPrint.Leijen


data NamingContext = Ctx
  { getNodeTypeName :: NodeId -> String
  , getEdgeTypeName :: EdgeId -> String
  }

makeNamingContext :: [(String, String)] -> NamingContext
makeNamingContext assocList = Ctx (nameForId . normalizeId) (nameForId . normalizeId)
  where
    normalizeId id = "I" ++ show id
    nameForId id =
      case lookup id assocList of
        Nothing -> error $ "Name for '" ++ id ++ "' not found."
        Just name -> takeWhile (/= '%') name


printDotGraph :: String -> String -> [String] -> [Doc] -> [Doc] -> [Doc] -> Doc
printDotGraph kind name props subgraphs nodes edges =
  let
    toLines = vcat . map (<> semi)
    body =
      toLines (map text props) <> line
      <> toLines subgraphs <> line
      <> toLines nodes <> line
      <> toLines edges
  in text kind <+> text name <+> braces (line <> body <> line)

printDigraph :: String -> [String] -> [Doc] -> [Doc] -> [Doc] -> Doc
printDigraph = printDotGraph "digraph"

printSubgraph :: String -> [String] -> [Doc] -> [Doc] -> [Doc] -> Doc
printSubgraph name props = printDotGraph "subgraph" ("cluster_" ++ name) (("label=" ++ show name) : props)


-- | Create a dotfile representation of the given state space, labeling states with their IDs
printStateSpace :: StateSpace (TGraph.CatM a b) (TypedGraphMorphism a b) -> Doc
printStateSpace stateSpace =
  printDigraph
    "stateSpace"
    []
    []
    (map prettyState . IntMap.toList $ states stateSpace)
    (map prettyTransition . Set.toList $ transitions stateSpace)

  where
    prettyState (key, (_, predicates)) =
      let label = int key <> char '\n' <> cat (punctuate (char '\n') . map text $ predicates)
      in printNode (int key) [text "label=" <> dquotes label]
    prettyTransition (from, to) = printEdge (int from) (int to) []


-- | Create a dotfile representation of the given typed graph, labeling nodes with their types
printTypedGraph :: NamingContext -> String -> TypedGraph a b -> Doc
printTypedGraph context graphName graph =
  printDigraph
    graphName
    ["node [shape=box]"]
    []
    (map prettyNode $ typedNodes graph)
    (map prettyEdge $ typedEdges graph)

  where
    prettyNode (node, nodeType) =
      let typeName = getNodeTypeName context nodeType
      in printNode (nodeId node) [text "label=" <> dquotes (text typeName)]

    prettyEdge (_, src, tgt, edgeType) =
      let typeName = getEdgeTypeName context edgeType
      in printEdge (nodeId src) (nodeId tgt) [text "label=" <> dquotes (text typeName)]

printSubTypedGraph :: NamingContext -> String -> TypedGraph a b -> Doc
printSubTypedGraph context graphName graph =
  printSubgraph
    graphName
    ["node [shape=box]"]
    []
    (map prettyNode $ typedNodes graph)
    (map prettyEdge $ typedEdges graph)

  where
    prettyNode (node, nodeType) =
      let label = text (show node ++ ":" ++ getNodeTypeName context nodeType)
      in printNode (nodeSubId graphName node) [text "label=" <> dquotes label]

    prettyEdge (edge, src, tgt, edgeType) =
      let label = text (show edge ++ ":" ++ getEdgeTypeName context edgeType)
      in printEdge (nodeSubId graphName src) (nodeSubId graphName tgt) [text "label=" <> dquotes label]

-- | Create a dotfile representation of the given typed graph morphism
printTypedGraphMorphism :: NamingContext -> String -> TypedGraphMorphism a b -> Doc
printTypedGraphMorphism context morphismName morphism =
  printDigraph
    morphismName
    []
    [ printSubTypedGraph context "src" (domain morphism)
    , printSubTypedGraph context "tgt" (codomain morphism)
    ]
    (map prettyNode $ typedNodes (mapping morphism))
    []

  where
    prettyNode (src, tgt) = printEdge (text "src" <> nodeId src) (text "tgt" <> nodeId tgt) [text "style=dotted"]

-- | Create a dotfile representation of the given graph rule
printGraphRule :: NamingContext -> String -> TypedGraphRule a b -> Doc
printGraphRule context ruleName rule =
  printDigraph
    ruleName
    []
    (printGraphRuleCore context leftName interfaceName rightName rule)
    (
     map (prettyLhsNode interfaceName leftName) (typedNodes (mapping (leftMorphism rule))) ++
     map (prettyRhsNode interfaceName rightName) (typedNodes (mapping (rightMorphism rule)))
    )
    []

  where
    (leftName, interfaceName, rightName) = getRuleName ruleName
    prettyLhsNode idSrc idTgt (src, tgt) = printEdge (text idSrc <> nodeId src) (text idTgt <> nodeId tgt) [text "style=dotted"]
    prettyRhsNode idSrc idTgt (src, tgt) = printEdge (text idTgt <> nodeId tgt) (text idSrc <> nodeId src) [text "dir=back,style=dotted"]

printSubGraphRule :: NamingContext -> String -> TypedGraphRule a b -> Doc
printSubGraphRule context ruleName rule =
  printSubgraph
    ruleName
    []
    (printGraphRuleCore context leftName interfaceName rightName rule)
    (
     map (prettyLhsNode interfaceName leftName) (typedNodes (mapping (leftMorphism rule))) ++
     map (prettyRhsNode interfaceName rightName) (typedNodes (mapping (rightMorphism rule)))
    )
    []

  where
    (leftName, interfaceName, rightName) = getRuleName ruleName
    prettyLhsNode idSrc idTgt (src, tgt) = printEdge (text idSrc <> nodeId src) (text idTgt <> nodeId tgt) [text "style=dotted"]
    prettyRhsNode idSrc idTgt (src, tgt) = printEdge (text idTgt <> nodeId tgt) (text idSrc <> nodeId src) [text "dir=back,style=dotted"]

printGraphRuleCore :: NamingContext -> String -> String -> String -> TypedGraphRule a b -> [Doc]
printGraphRuleCore context leftName interfaceName rightName rule =
  [ printSubTypedGraph context leftName (codomain (leftMorphism rule))
  , printSubTypedGraph context interfaceName (domain (leftMorphism rule))
  , printSubTypedGraph context rightName (codomain (rightMorphism rule))
  ]

getRuleName :: String -> (String, String, String)
getRuleName ruleName = (ruleName ++ "L", ruleName ++ "K", ruleName ++ "R")

-- | Create a dotfile representation of the given second-order rule
printSndOrderRule :: NamingContext -> String -> SndOrderRule a b -> Doc
printSndOrderRule context ruleName rule =
  printDigraph
    ruleName
    []
    [ printSubGraphRule context leftName (codomain (leftMorphism rule))
    , printSubGraphRule context interfaceName (domain (leftMorphism rule))
    , printSubGraphRule context rightName (codomain (rightMorphism rule))
    ]
    (
    map (mapNode False (ruleName ++ "K" ++ "L") (ruleName ++ "L" ++ "L")) (typedNodes (mapping (mappingLeft (leftMorphism rule)))) ++
    map (mapNode False (ruleName ++ "K" ++ "K") (ruleName ++ "L" ++ "K")) (typedNodes (mapping (mappingInterface (leftMorphism rule)))) ++
    map (mapNode False (ruleName ++ "K" ++ "R") (ruleName ++ "L" ++ "R")) (typedNodes (mapping (mappingRight (leftMorphism rule)))) ++
    map (mapNode True  (ruleName ++ "K" ++ "L") (ruleName ++ "R" ++ "L")) (typedNodes (mapping (mappingLeft (rightMorphism rule)))) ++
    map (mapNode True  (ruleName ++ "K" ++ "K") (ruleName ++ "R" ++ "K")) (typedNodes (mapping (mappingInterface (rightMorphism rule)))) ++
    map (mapNode True  (ruleName ++ "K" ++ "R") (ruleName ++ "R" ++ "R")) (typedNodes (mapping (mappingRight (rightMorphism rule))))
    )
    []

  where
    leftName = ruleName++"L"
    interfaceName = ruleName++"K"
    rightName = ruleName++"R"

    mapNode True idSrc idTgt (src, tgt) =
      printEdge (text idSrc <> nodeId src) (text idTgt <> nodeId tgt) [text "style=dashed"]
    mapNode False idSrc idTgt (src, tgt) =
      printEdge (text idTgt <> nodeId tgt) (text idSrc <> nodeId src) [text "dir=back,style=dashed"]

printNode :: Doc -> [Doc] -> Doc
printNode id attributes =
  id <> (brackets . align . cat) (punctuate comma attributes)

printEdge :: Doc -> Doc -> [Doc] -> Doc
printEdge srcId tgtId attributes =
  srcId <+> text "->" <+> tgtId <+> (brackets . align . cat) (punctuate comma attributes) <> semi

nodeSubId :: String -> NodeId -> Doc
nodeSubId graphName (NodeId n) =
  text graphName <> int n

nodeId :: NodeId -> Doc
nodeId (NodeId n) =
  int n

edgeSubId :: String -> EdgeId -> Doc
edgeSubId graphName (EdgeId n) =
  text graphName <> int n

edgeId :: EdgeId -> Doc
edgeId (EdgeId n) =
  int n
