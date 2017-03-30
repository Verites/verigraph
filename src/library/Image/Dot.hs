module Image.Dot where

import           Abstract.DPO.StateSpace
import           Abstract.Morphism
import           Graph.Graph              hiding (Node (..))
import           SndOrder.Morphism
import           SndOrder.Rule
import           TypedGraph.DPO.GraphRule
import           TypedGraph.Graph
import           TypedGraph.Morphism

import qualified Data.IntMap              as IntMap
import qualified Data.Set                 as Set
import           Text.PrettyPrint.Leijen


data NamingContext = Ctx
  { getNodeTypeName :: NodeId -> String
  , getEdgeTypeName :: EdgeId -> String
  }

makeNamingContext :: [(String, String)] -> NamingContext
makeNamingContext assocList =
  let
    normalizeId id =
      "I" ++ show id

    nameForId id =
      case lookup id assocList of
        Nothing ->
          error $ "Name for '" ++ id ++ "' not found."

        Just name ->
          takeWhile (/= '%') name
  in
    Ctx (nameForId . normalizeId) (nameForId . normalizeId)

printDotGraph :: String -> String -> [String] -> [Doc] -> [Doc] -> [Doc] -> Doc
printDotGraph kind name props subgraphs nodes edges =
  let
    toLines =
      vcat . map (<> semi)

    body =
      toLines (map text props) <> line
      <> toLines subgraphs <> line
      <> toLines nodes <> line
      <> toLines edges
  in
    text kind <+> text name <+> braces (line <> body <> line)


printDigraph :: String -> [String] -> [Doc] -> [Doc] -> [Doc] -> Doc
printDigraph =
  printDotGraph "digraph"


printSubgraph :: String -> [String] -> [Doc] -> [Doc] -> [Doc] -> Doc
printSubgraph name props =
  printDotGraph "subgraph" ("cluster_" ++ name) (("label=" ++ show name) : props)


-- | Create a dotfile representation of the given state space, labeling states with their IDs
printStateSpace :: StateSpace (TypedGraphMorphism a b) -> Doc
printStateSpace stateSpace =
  printDigraph
    "stateSpace"
    []
    []
    (map prettyState . IntMap.toList $ states stateSpace)
    (map prettyTransition . Set.toList $ transitions stateSpace)

  where
    prettyState (key, (_, predicates)) =
      let
        label =
          int key <> char '\n' <> cat (punctuate (char '\n') . map text $ predicates)
      in
        printNode (int key) [text "label=" <> dquotes label]

    prettyTransition (from, to) =
      printEdge (int from) (int to) []


-- | Create a dotfile representation of the given typed graph, labeling nodes with their types
printTypedGraph :: NamingContext -> String -> TypedGraph a b -> Doc
printTypedGraph context graphName graph =
  printDigraph
    graphName
    ["node [shape=box]"]
    []
    (map prettyNode $ nodesWithType graph)
    (map prettyEdge $ edgesWithType graph)

  where
    prettyNode (node, nodeType) =
      let
        typeName =
          getNodeTypeName context nodeType
      in
        printNode (nodeId node) [text "label=" <> dquotes (text typeName)]

    prettyEdge (_, src, tgt, edgeType) =
      let
        typeName =
          getEdgeTypeName context edgeType
      in
        printEdge (nodeId src) (nodeId tgt) [text "label=" <> dquotes (text typeName)]

printSubTypedGraph :: NamingContext -> String -> TypedGraph a b -> Doc
printSubTypedGraph context graphName graph =
  printSubgraph
    graphName
    ["node [shape=box]"]
    []
    (map prettyNode $ nodesWithType graph)
    (map prettyEdge $ edgesWithType graph)

  where
    prettyNode (node, nodeType) =
      let
        label =
          text (show node ++ ":" ++ getNodeTypeName context nodeType)
      in
        printNode (nodeSubId graphName node) [text "label=" <> dquotes label]

    prettyEdge (edge, src, tgt, edgeType) =
      let
        label =
          text (show edge ++ ":" ++ getEdgeTypeName context edgeType)
      in
        printEdge (nodeSubId graphName src) (nodeSubId graphName tgt) [text "label=" <> dquotes label]

-- | Create a dotfile representation of the given typed graph morphism
printTypedGraphMorphism :: NamingContext -> String -> TypedGraphMorphism a b -> Doc
printTypedGraphMorphism context morphismName morphism =
  printDigraph
    morphismName
    []
    [ printSubTypedGraph context "src" (domain morphism)
    , printSubTypedGraph context "tgt" (codomain morphism)
    ]
    (map mapNode $ nodesWithType (mapping morphism))
    []

  where
    mapNode (src, tgt) =
      printEdge (text "src" <> nodeId src) (text "tgt" <> nodeId tgt) [text "style=dotted"]

-- | Create a dotfile representation of the given graph rule
printGraphRule :: NamingContext -> String -> GraphRule a b -> Doc
printGraphRule context ruleName rule =
  printDigraph
    ruleName
    []
    [ printSubTypedGraph context leftName (codomain (getLHS rule))
    , printSubTypedGraph context interfaceName (domain (getLHS rule))
    , printSubTypedGraph context rightName (codomain (getRHS rule))
    ]
    (concat
      [ map (mapNode False interfaceName leftName) $ nodesWithType (mapping (getLHS rule))
      , map (mapNode True interfaceName rightName) $ nodesWithType (mapping (getRHS rule))
      ])
    []

  where
    leftName = ruleName++"L"
    interfaceName = ruleName++"K"
    rightName = ruleName++"R"

    mapNode False idSrc idTgt (src, tgt) =
      printEdge (text idSrc <> nodeId src) (text idTgt <> nodeId tgt) [text "style=dotted"]
    mapNode True idSrc idTgt (src, tgt) =
      printEdge (text idTgt <> nodeId tgt) (text idSrc <> nodeId src) [text "dir=back,style=dotted"]

printSubGraphRule :: NamingContext -> String -> GraphRule a b -> Doc
printSubGraphRule context ruleName rule =
  printSubgraph
    ruleName
    []
    [ printSubTypedGraph context leftName (codomain (getLHS rule))
    , printSubTypedGraph context interfaceName (domain (getLHS rule))
    , printSubTypedGraph context rightName (codomain (getRHS rule))
    ]
    (concat
      [ map (mapNode False interfaceName leftName) $ nodesWithType (mapping (getLHS rule))
      , map (mapNode True interfaceName rightName) $ nodesWithType (mapping (getRHS rule))
      ])
    []

  where
    leftName = ruleName++"L"
    interfaceName = ruleName++"K"
    rightName = ruleName++"R"

    mapNode False idSrc idTgt (src, tgt) =
      printEdge (text idSrc <> nodeId src) (text idTgt <> nodeId tgt) [text "style=dotted"]
    mapNode True idSrc idTgt (src, tgt) =
      printEdge (text idTgt <> nodeId tgt) (text idSrc <> nodeId src) [text "dir=back,style=dotted"]

-- | Create a dotfile representation of the given snd order rule
printSndOrderRule :: NamingContext -> String -> SndOrderRule a b -> Doc
printSndOrderRule context ruleName rule =
  printDigraph
    ruleName
    []
    [ printSubGraphRule context leftName (codomain (getLHS rule))
    , printSubGraphRule context interfaceName (domain (getLHS rule))
    , printSubGraphRule context rightName (codomain (getRHS rule))
    ]
    (
    (map (mapNode False (ruleName++"K"++"L") (ruleName++"L"++"L")) $ nodesWithType (mapping (mappingLeft (getLHS rule)))) ++
    (map (mapNode False (ruleName++"K"++"K") (ruleName++"L"++"K")) $ nodesWithType (mapping (mappingInterface (getLHS rule)))) ++
    (map (mapNode False (ruleName++"K"++"R") (ruleName++"L"++"R")) $ nodesWithType (mapping (mappingRight (getLHS rule)))) ++
    (map (mapNode True (ruleName++"K"++"L") (ruleName++"R"++"L")) $ nodesWithType (mapping (mappingLeft (getRHS rule)))) ++
    (map (mapNode True (ruleName++"K"++"K") (ruleName++"R"++"K")) $ nodesWithType (mapping (mappingInterface (getRHS rule)))) ++
    (map (mapNode True (ruleName++"K"++"R") (ruleName++"R"++"R")) $ nodesWithType (mapping (mappingRight (getRHS rule))))
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
