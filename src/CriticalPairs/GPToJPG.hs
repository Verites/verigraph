module CriticalPairs.GPToJPG (gind,write) where

import CriticalPairs.GraphPart

import Data.Text.Lazy (Text, pack, unpack)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (
  GraphvizParams,
  GlobalAttributes(
    GraphAttrs,
    NodeAttrs
    ),
  X11Color(Transparent, White),
  nonClusteredParams,
  globalAttributes,
  fmtNode,
  fmtEdge,
  graphToDot
  )
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete (
  DPoint(DVal),
  Attribute(
    Margin,
    Pad,
    Center,
    BgColor,
    FontSize,
    Shape,
    Label,
    ViewPort,
    RankDir,
    Style,
    FillColor
    ),
  Shape(Circle, BoxShape),
  Label(StrLabel),
  ViewPort(VP),
  RankDir(FromLeft),
  StyleName(Filled),
  StyleItem(SItem),
  toWColor,
  wVal,
  hVal,
  zVal,
  focus
  )

gnomeParams :: GraphvizParams n Text Text () Text
gnomeParams = nonClusteredParams {
  globalAttributes = ga,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    ga = [
      GraphAttrs [
         RankDir FromLeft,
         BgColor [toWColor Transparent]
         ],
      NodeAttrs [
        Shape BoxShape,
        FillColor [toWColor White],
        Style [SItem Filled []]
        ]
      ]

    fn (n,l) = [(Label . StrLabel) l]
    fe (f,t,l) = [(Label . StrLabel) l]

write g n = unpack $ renderDot $ toDot $ graphToDot gnomeParams (g!!n)

--grafo com indices
gind :: [EqClassGraph] -> [Gr Text Text]
gind gr = getGraphs gr gr 0

enter a = if a == "" then "" else "\n"

mergeNameNodes :: [Node] -> String -> String
mergeNameNodes []     a = a
mergeNameNodes (x:xs) a = mergeNameNodes xs (a++(enter a)++(show $ nname x)++"{"++(ngsource x)++"}")

getNodesPart :: [[Node]] -> [Node] -> Int -> (Int, Text)
getNodesPart orig x  n = (getInd orig (head x) 0, pack ((mergeNameNodes x "")++"\nTipo: "++(show $ ntype $ head x)))

getNodes :: [[Node]] -> [[Node]] -> Int -> [(Int, Text)]
getNodes _    []     _ = []
getNodes orig (x:xs) n = (getNodesPart orig x n):(getNodes orig xs (n+1))

mergeNameEdges :: [Edge] -> String -> String
mergeNameEdges []     a = a
mergeNameEdges (x:xs) a = mergeNameEdges xs (a++(enter a)++(show $ label x)++"{"++(egsource x)++"}")

getEdgesPart :: [[Node]] -> [Edge] -> (Int, Int, Text)
getEdgesPart a x = ((getInd a (source $ head x) 0), (getInd a (target $ head x) 0), pack (mergeNameEdges x ""++"\nTipo: "++(show $ etype $ head x)))

getEdges :: [[Node]] -> [[Edge]] -> [(Int, Int, Text)]
getEdges _ []     = []
getEdges a (x:xs) = (getEdgesPart a x) : (getEdges a xs)

--recebe um grafo do GraphPart e transforma para o formato .dot
getGraphs :: [EqClassGraph] -> [EqClassGraph] -> Int -> [Gr Text Text]
getGraphs _  []     _ = []
getGraphs gr (x:xs) n = (mkGraph (getNodes nods nods 1) edgs):(getGraphs gr xs (n+1))
    where
       edgs = if (snd $ gr!!n) == [[]] then [] else (getEdges nods (snd $ gr!!n))
       nods = fst $ gr!!n
