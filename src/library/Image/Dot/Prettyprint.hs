{-# LANGUAGE OverloadedStrings #-}
{-| Basic building blocks for writing graphs with the Dot syntax.

 This contains only the basic building blocks of to Dot syntax,
 and it does __not__ provide conventions to print particular
 kinds of graphs.

 This module is intended to be imported qualified, e.g.
 >  import qualified Data.Map as Map


-}
module Image.Dot.Prettyprint
  (graph, digraph, node, undirEdge, dirEdge, attrList, subgraph, anonSubgraph) where

import           Data.Text.Prettyprint.Doc

-- | Pretty prints a (undirected) graph with the given name and list of statements.
graph :: Doc ann -> [Doc ann] -> Doc ann
graph = genericGraph "graph"

-- | Pretty prints a directed graph with the given name and list of statements.
digraph :: Doc ann -> [Doc ann] -> Doc ann
digraph = genericGraph "digraph"

-- | Pretty prints a subgraph with the given name and list of statements.
subgraph :: Doc ann -> [Doc ann] -> Doc ann
subgraph = genericGraph "subgraph"

-- | Pretty prints an anonymous subgraph with the given list of statements.
anonSubgraph :: [Doc ann] -> Doc ann
anonSubgraph = braces . align . sep . punctuate semi

genericGraph :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
genericGraph kind name body = vsep
  [ hsep [ kind, name, lbrace ]
  , indent 2 . vsep $ punctuate semi body
  , rbrace ]

-- | Pretty prints a node statement with given identifier and list of attributes.
node :: Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
node nId []    = nId
node nId attrs = nId <+> attrList attrs

-- | Pretty prints an undirected edge with given source, target and list of attributes.
undirEdge :: Doc ann -> Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
undirEdge = genericEdge "--"

-- | Pretty prints a directed edge with given source, target and list of attributes.
dirEdge :: Doc ann -> Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
dirEdge = genericEdge "->"

genericEdge :: Doc ann -> Doc ann -> Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
genericEdge kind src tgt []    = hang 2 $ fillSep [ src, kind, tgt ]
genericEdge kind src tgt attrs = hang 2 $ fillSep [ src, kind, tgt, attrList attrs ]

-- | Pretty prints a list of attributes.
attrList :: [(Doc ann, Doc ann)] -> Doc ann
attrList = fuse Shallow . brackets . align . sep . punctuate comma . map attr
  where attr (key, val) = hcat [key, "=", val]
