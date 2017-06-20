{-# LANGUAGE Arrows #-}

module XML.GPSReader.GTXLParseIn where

import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust, fromMaybe, mapMaybe)
import           Data.Tree.NTree.TypeDefs
import           System.Directory
import           System.FilePath
import           Text.XML.HXT.Core

import           Abstract.Category.AdhesiveHLR
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraphRule
import qualified Data.Graphs                   as G
import           Data.Graphs.Morphism          as GM
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph      as GR
import           Rewriting.DPO.TypedGraphRule
import qualified XML.Formulas                  as F
import           XML.GGXParseIn
import           XML.GGXReader.SndOrder
import           XML.GGXReader.Span
import           XML.ParsedTypes
import           XML.Utilities
import           XML.XMLUtilities

readGPR fileName = do
  [rule] <- readRule fileName  
  return (fileName,rule)

readRule fileName = runX (parseXML fileName >>> parseGPRRule)

parseGPRRule = atTag "gxl" >>> atTag "graph" >>>
  proc graph -> do
    nodes <- listA parseGPRNode -< graph
    edges <- listA parseGPREdge -< graph
    let nodesId = zip nodes [0..]
    let edgesId = zip edges [0..]
    returnA -< (nodesId,edgesId)

parseGPRNode = atTag "node" >>>
  proc nodes -> do
    x <- getAttrValue "id" -< nodes
    returnA -< x

parseGPREdge = atTag "edge" >>>
  proc edges -> do
    from <- getAttrValue "from" -< edges
    to <- getAttrValue "to" -< edges
    labels <- listA parseGPREdgeLabel -< edges
    returnA -< (from,to,labels)

parseGPREdgeLabel = atTag "attr" >>>
  proc attrs -> do
    _  <- isA (\str -> not (str == "layout")) <<< getAttrValue "name" -< attrs
    string <- (getChildren >>> hasName "string" /> getText) -< attrs
    returnA -< string
