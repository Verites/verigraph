{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-| Basic building blocks for writing graphs with the Dot syntax.

This contains only the basic building blocks of to Dot syntax,
and it does __not__ provide conventions to print particular
kinds of graphs.
-}
module Image.Dot.TypedGraph 
  ( NamingContext(..)
  , makeNamingContext
  , typedGraph
  , typedGraphMorphism
  , graphRule
  , sndOrderRule
  ) where

import Data.Maybe (fromMaybe)
import           Data.Text.Prettyprint.Doc (Pretty(..), Doc, (<>), (<+>))
import qualified Data.Text.Prettyprint.Doc as PP

import           Abstract.Category
import           Category.TypedGraphRule
import qualified Image.Dot.Prettyprint as Dot
import qualified Data.Graphs as Graph
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph
import           Rewriting.DPO.TypedGraphRule


data NamingContext n e = Ctx
  { getNodeTypeName :: Graph.NodeInContext (Maybe n) (Maybe e) -> String
  , getEdgeTypeName :: Graph.EdgeInContext (Maybe n) (Maybe e) -> String
  }

-- TODO: move this to XML parsing module
makeNamingContext :: [(String, String)] -> NamingContext n e
makeNamingContext assocList =
  Ctx (nameForId . normalizeId . nodeId) (nameForId . normalizeId . edgeId)
  where
    nodeId (node, _) = Graph.nodeId node
    edgeId (_, edge, _) = Graph.edgeId edge
    normalizeId id = "I" ++ show id
    nameForId id =
      case lookup id assocList of
        Nothing -> error $ "Name for '" ++ id ++ "' not found."
        Just name -> takeWhile (/= '%') name

-- | Create a dotfile representation of the given typed graph, labeling nodes with their types
typedGraph :: NamingContext n e -> Doc ann -> TypedGraph n e -> Doc ann
typedGraph context name graph = Dot.digraph name (typedGraphBody context name graph)

typedGraphBody :: NamingContext n e -> Doc ann -> TypedGraph n e -> [Doc ann]
typedGraphBody context idPrefix graph =
  nodeAttrs : map prettyNode (nodes graph) ++ map prettyEdge (edges graph)
  where
    nodeAttrs = "node" <+> Dot.attrList [("shape", "box")]
    prettyNode (Node n _, typeId) = Dot.node (idPrefix <> pretty n) [("label", PP.dquotes typeName)]
      where typeName = fromMaybe PP.emptyDoc
              $ pretty . getNodeTypeName context <$> Graph.lookupNodeInContext typeId (typeGraph graph)
    prettyEdge (Edge _ src tgt _, typeId) =
      Dot.dirEdge (idPrefix <> pretty src) (idPrefix <> pretty tgt) [("label", PP.dquotes typeName)]
      where typeName = fromMaybe PP.emptyDoc
              $ pretty . getEdgeTypeName context <$> Graph.lookupEdgeInContext typeId (typeGraph graph)

-- | Create a dotfile representation of the given typed graph morphism
typedGraphMorphism :: NamingContext n e -> Doc ann -> TypedGraphMorphism n e -> Doc ann
typedGraphMorphism context name morphism = Dot.digraph name (typedGraphMorphismBody context morphism)

typedGraphMorphismBody :: NamingContext n e -> TypedGraphMorphism n e -> [Doc ann]
typedGraphMorphismBody context morphism =
  Dot.subgraph "dom" (typedGraphBody context "dom" (domain morphism))
  : Dot.subgraph "cod" (typedGraphBody context "cod" (codomain morphism))
  : map (prettyNodeMapping [("style", "dotted")] "dom" "cod") (nodeMapping morphism)
  
prettyNodeMapping :: (Pretty a) => [(Doc ann, Doc ann)] -> Doc ann -> Doc ann -> (a, a) -> Doc ann
prettyNodeMapping attrs idSrc idTgt (src, tgt) =
  Dot.dirEdge (idSrc <> pretty src) (idTgt <> pretty tgt) attrs

-- | Create a dotfile representation of the given graph rule
graphRule :: NamingContext n e -> Doc ann -> TypedGraphRule n e -> Doc ann
graphRule context ruleName rule = Dot.digraph ruleName (graphRuleBody context ruleName rule)

graphRuleBody :: NamingContext n e -> Doc ann -> TypedGraphRule n e -> [Doc ann]
graphRuleBody context ruleName rule =
  Dot.subgraph leftName (typedGraphBody context leftName (leftObject rule))
  :  Dot.subgraph interfaceName (typedGraphBody context interfaceName (interfaceObject rule))
  :  Dot.subgraph rightName (typedGraphBody context rightName (rightObject rule))
  :  map (prettyNodeMapping [("style", "dotted")] interfaceName leftName)
      (nodeMapping $ leftMorphism rule)
  ++ map (prettyNodeMapping [("style", "dotted"), ("dir", "back")] interfaceName rightName)
      (nodeMapping $ rightMorphism rule)
  where
    (leftName, interfaceName, rightName) = ("L_" <> ruleName, "K_" <> ruleName, "R_" <> ruleName)

-- | Create a dotfile representation of the given second-order rule
sndOrderRule :: NamingContext n e -> Doc ann -> SndOrderRule n e -> Doc ann
sndOrderRule context ruleName rule = Dot.digraph ruleName (sndOrderRuleBody context ruleName rule)

sndOrderRuleBody :: NamingContext n e -> Doc ann -> SndOrderRule n e -> [Doc ann]
sndOrderRuleBody context ruleName rule =
  Dot.subgraph leftName (graphRuleBody context leftName (leftObject rule))
  :  Dot.subgraph interfaceName (graphRuleBody context interfaceName (interfaceObject rule))
  :  Dot.subgraph rightName (graphRuleBody context rightName (rightObject rule))
  :  map (prettyNodeMapping [("style", "dashed")] (ruleName <> "KL") (ruleName <> "LL"))
      (nodeMapping . mappingLeft $ leftMorphism rule)
  ++ map (prettyNodeMapping [("style", "dashed")] (ruleName <> "KK") (ruleName <> "LK"))
      (nodeMapping . mappingInterface $ leftMorphism rule)
  ++ map (prettyNodeMapping [("style", "dashed")] (ruleName <> "KR") (ruleName <> "LR"))
      (nodeMapping . mappingRight $ leftMorphism rule)
  ++ map (prettyNodeMapping [("style", "dashed"), ("dir", "back")] (ruleName <> "KL") (ruleName <> "RL"))
      (nodeMapping . mappingLeft $ rightMorphism rule)
  ++ map (prettyNodeMapping [("style", "dashed"), ("dir", "back")] (ruleName <> "KK") (ruleName <> "RK"))
      (nodeMapping . mappingInterface $ rightMorphism rule)
  ++ map (prettyNodeMapping [("style", "dashed"), ("dir", "back")] (ruleName <> "KR") (ruleName <> "RR"))
      (nodeMapping . mappingRight $ rightMorphism rule)
  where
    (leftName, interfaceName, rightName) = ("L_" <> ruleName, "K_" <> ruleName, "R_" <> ruleName)
    prettyNodeMapping attrs idSrc idTgt (src, tgt) =
      Dot.dirEdge (idSrc <> pretty src) (idTgt <> pretty tgt) attrs
