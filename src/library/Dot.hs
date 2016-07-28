module Dot where

import Abstract.DPO.StateSpace
import Graph.Graph
import TypedGraph.Graph
import TypedGraph.Morphism

import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Text.PrettyPrint.Leijen


data NamingContext = Ctx
  { getNodeTypeName :: NodeId -> String
  , getEdgeTypeName :: EdgeId -> String
  }


printDigraph :: String -> [String] -> [Doc] -> [Doc] -> Doc
printDigraph name props nodes edges =
  let
    toLines =
      vcat . map (<> semi)

    body =
      toLines (map text props) <> line
      <> toLines nodes <> line
      <> toLines edges
  in
    text "digraph" <+> text name <+> braces (line <> body <> line)


-- | Create a dotfile representation of the given state space, labeling states with their IDs
printStateSpace :: StateSpace (TypedGraphMorphism a b) -> Doc
printStateSpace stateSpace =
  printDigraph
    "stateSpace"
    []
    (map prettyState . IntMap.toList $ states stateSpace)
    (map prettyTransition . Set.toList $ transitions stateSpace)

  where
    prettyState (key, (_, predicates)) =
      let
        label =
          int key <> newline <> cat (punctuate newline . map text $ predicates)
        newline =
          text "\\n"
      in
        int key <+> brackets (text "label=" <> dquotes label)

    prettyTransition (from, to) =
      int from <+> text "->" <+> int to


-- | Create a dotfile representation of the given typed graph, labeling nodes with their types
printTypedGraph :: NamingContext -> String -> TypedGraph a b -> Doc
printTypedGraph context graphName graph =
  printDigraph
    graphName
    ["node [shape=box]"]
    (map prettyNode $ nodesWithType graph)
    (map prettyEdge $ edgesWithType graph)

  where
    prettyNode (node, nodeType) =
      let
        typeName =
          getNodeTypeName context nodeType
      in
        nodeId node <+> brackets
          (text "label=" <> dquotes (text typeName) <> semi)

    prettyEdge (_, src, tgt, edgeType) =
      let
        typeName =
          getEdgeTypeName context edgeType
      in
        nodeId src <+> text "->" <+> nodeId tgt <+> brackets
          (text "label=" <> dquotes (text typeName) <> semi)


nodeId :: NodeId -> Doc
nodeId (NodeId n) =
  int n

edgeId :: EdgeId -> Doc
edgeId (EdgeId n) =
  int n
