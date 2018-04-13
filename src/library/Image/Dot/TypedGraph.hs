{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Data.Text.Prettyprint.Doc    (Doc, Pretty (..), (<+>), (<>))
import qualified Data.Text.Prettyprint.Doc    as PP

import           Abstract.Category
import           Category.TypedGraphRule
import qualified Data.Graphs                  as Graph
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import qualified Image.Dot.Prettyprint        as Dot
import           Rewriting.DPO.TypedGraph
import           Rewriting.DPO.TypedGraphRule

data NamingContext n e ann = Ctx
  { getNodeTypeName :: Graph.NodeInContext (Maybe n) (Maybe e) -> Doc ann
  , getEdgeTypeName :: Graph.EdgeInContext (Maybe n) (Maybe e) -> Doc ann
  , getNodeName     :: Doc ann -> NodeInContext n e -> Doc ann
  , getNodeLabel    :: Doc ann -> NodeInContext n e -> Maybe (Doc ann)
  , getEdgeLabel    :: Doc ann -> EdgeInContext n e -> Maybe (Doc ann)
  }

-- TODO: move this to XML parsing module
makeNamingContext :: [(String, String)] -> NamingContext n e ann
makeNamingContext assocList = Ctx
  { getNodeTypeName = nameForId . normalizeId . nodeId
  , getEdgeTypeName = nameForId . normalizeId . edgeId
  , getNodeName = \idPrefix (Node n _, _, _) -> idPrefix <> pretty n
  , getNodeLabel = \_ (_,Node ntype _,_) -> Just . nameForId $ normalizeId ntype
  , getEdgeLabel = \_ (_,_,Edge etype _ _ _,_) -> Just . nameForId $ normalizeId etype
  }
  where
    nodeId (node, _) = Graph.nodeId node
    edgeId (_, edge, _) = Graph.edgeId edge
    normalizeId id = "I" ++ show id
    nameForId id =
      case lookup id assocList of
        Nothing -> error $ "Name for '" ++ id ++ "' not found."
        Just name -> pretty $ takeWhile (/= '%') name

-- | Create a dotfile representation of the given typed graph, labeling nodes with their types
typedGraph :: NamingContext n e ann -> Doc ann -> TypedGraph n e -> Doc ann
typedGraph context name graph = Dot.digraph name (typedGraphBody context name graph)

typedGraphBody :: NamingContext n e ann -> Doc ann -> TypedGraph n e -> [Doc ann]
typedGraphBody context idPrefix graph =
  nodeAttrs : map prettyNode (nodes graph) ++ map prettyEdge (edges graph)
  where
    nodeAttrs = "node" <+> Dot.attrList [("shape", "box")]
    prettyNode (Node n _, _) = Dot.node name attrs
      where
        node = lookupNodeInContext n graph
        Just name = getNodeName context idPrefix <$> node
        attrs = case getNodeLabel context idPrefix =<< node of
          Nothing -> []
          Just label -> [("label", PP.dquotes label)]
    prettyEdge (Edge e src tgt _, _) =
      Dot.dirEdge srcName tgtName attrs
      where
        Just srcName = getNodeName context idPrefix <$> lookupNodeInContext src graph
        Just tgtName = getNodeName context idPrefix <$> lookupNodeInContext tgt graph
        attrs = case getEdgeLabel context idPrefix =<< lookupEdgeInContext e graph of
          Nothing -> []
          Just label -> [("label", PP.dquotes label)]

-- | Create a dotfile representation of the given typed graph morphism
typedGraphMorphism :: NamingContext n e ann -> Doc ann -> TypedGraphMorphism n e -> Doc ann
typedGraphMorphism context name morphism = Dot.digraph name (typedGraphMorphismBody context morphism)

typedGraphMorphismBody :: NamingContext n e ann -> TypedGraphMorphism n e -> [Doc ann]
typedGraphMorphismBody context morphism =
  Dot.subgraph "dom" (typedGraphBody context "dom" (domain morphism))
  : Dot.subgraph "cod" (typedGraphBody context "cod" (codomain morphism))
  : map (prettyNodeMapping [("style", "dotted")] "dom" "cod") (nodeMapping morphism)

prettyNodeMapping :: (Pretty a) => [(Doc ann, Doc ann)] -> Doc ann -> Doc ann -> (a, a) -> Doc ann
prettyNodeMapping attrs idSrc idTgt (src, tgt) =
  Dot.dirEdge (idSrc <> pretty src) (idTgt <> pretty tgt) attrs

-- | Create a dotfile representation of the given graph rule
graphRule :: NamingContext n e ann -> Doc ann -> TypedGraphRule n e -> Doc ann
graphRule context ruleName rule = Dot.digraph ruleName (graphRuleBody context ruleName rule)

graphRuleBody :: NamingContext n e ann -> Doc ann -> TypedGraphRule n e -> [Doc ann]
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
sndOrderRule :: NamingContext n e ann -> Doc ann -> SndOrderRule n e -> Doc ann
sndOrderRule context ruleName rule = Dot.digraph ruleName (sndOrderRuleBody context ruleName rule)

sndOrderRuleBody :: NamingContext n e ann -> Doc ann -> SndOrderRule n e -> [Doc ann]
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
