{-# LANGUAGE Arrows #-}

module XML.GPSReader.GTXLReader where

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
import           XML.GPSReader.GTXLParseIn
import           XML.ParsedTypes
import           XML.Utilities
import           XML.XMLUtilities

readGrammar fileName = do
  print fileName
  files <- getDirectoryContents fileName
  
  --let stateNames = filter (\name -> takeExtension name == ".gst") files
  let ruleNames = filter (\name -> takeExtension name == ".gpr") files
      ruleFileNames = map (\n -> fileName ++ "/" ++ n) ruleNames
  
  t <- mapM readGPR ruleFileNames
  
  return t
