module XML.GGXReader
 ( readGrammar,
   readGGName,
   readName,
   readNames,
   readTypeGraph,
   readRules,
   readSequences,
   instantiateRule,
   instantiateSpan
   ) where

import           Abstract.DPO
import           Abstract.Morphism
import           Abstract.Valid
import qualified Data.List                as L
import           Data.Maybe               (fromMaybe, mapMaybe)
import           Data.String.Utils        (startswith)
import qualified Graph.Graph              as G
import qualified Graph.GraphGrammar       as GG
import           Graph.GraphMorphism      as GM
import           Graph.GraphRule          as GR
import           Graph.TypedGraphMorphism
import           Text.XML.HXT.Core        hiding (left,right)
import           XML.GGXParseIn
import           XML.GGXSndOrderReader
import           XML.ParsedTypes
import           XML.XMLUtilities
import           XML.Utilities

readGrammar :: String -> IO (GG.GraphGrammar a b)
readGrammar fileName = do
  parsedTypeGraphs <- readTypeGraph fileName
  let parsedTypeGraph = case parsedTypeGraphs of
                         []    -> error "error, type graph not found"
                         ptg:_ -> ptg
  _ <- parsedTypeGraph `seq` return ()

  parsedRules <- readRules fileName
  
  let (sndOrdRules, fstOrdRules) = L.partition (\((x,_,_,_),_) -> startswith "2rule_" x) parsedRules
      rulesNames = map (\((x,_,_,_),_) -> x) fstOrdRules
      rules = map (instantiateRule parsedTypeGraph) fstOrdRules
  
  _ <- (case L.elemIndices False (map valid rules) of
          []  -> []
          [a] -> error $ "Rule " ++ show a ++ " is not valid"
          l   -> error $ "Rules " ++ show l ++ " are not valid"
          ) `seq` return ()

  let typeGraph = if L.null rules
                    then error "Not found first order rules, at least one is needed"
                    else codomain . domain . left $ head rules
      initGraph = GM.empty typeGraph typeGraph
      sndOrderRules = instantiateSndOrderRules parsedTypeGraph sndOrdRules
  
  return $ GG.graphGrammar initGraph (zip rulesNames rules) sndOrderRules

readGGName :: String -> IO String
readGGName fileName = do
  name <- readName fileName
  let ret = case name of
              n:_ -> n
              _   -> "GraGra"
  return ret

-- | Reads the names of node/edge types and NACs, which are necessary when reexporting this grammar.
readNames :: String -> IO [(String,String)]
readNames fileName = (++) <$> readTypeNames fileName <*> readNacNames fileName

readName :: String -> IO [String]
readName fileName = runX (parseXML fileName >>> parseGGName)

readTypeGraph :: String -> IO[ParsedTypeGraph]
readTypeGraph fileName = runX (parseXML fileName >>> parseTypeGraph)

readNacNames :: String -> IO [(String,String)]
readNacNames fileName = concat <$> runX (parseXML fileName >>> parseNacNames)

readTypeNames :: String -> IO [(String,String)]
readTypeNames fileName = concat <$> runX (parseXML fileName >>> parseNames)

readRules :: String -> IO[RuleWithNacs]
readRules fileName = runX (parseXML fileName >>> parseRule)

readSequences :: GG.GraphGrammar a b -> String -> IO [(String, [GR.GraphRule a b])]
readSequences grammar fileName = map (expandSequence grammar) <$> runX (parseXML fileName >>> parseRuleSequence)

expandSequence :: GG.GraphGrammar a b -> Sequence -> (String, [GR.GraphRule a b])
expandSequence grammar (name,s) = (name, mapMaybe lookupRule . concat $ map expandSub s)
  where
    expandSub (i, s) = concat $ replicate i $ concatMap expandItens s
    expandItens (i, r) = replicate i r
    lookupRule name = L.lookup name (GG.rules grammar)

instantiateTypeGraph :: ParsedTypeGraph -> G.Graph a b
instantiateTypeGraph (nodes, edges) = graphWithEdges
  where
    getNodeType = G.NodeId . toN . (lookupNodes nodes)
    trd (_,_,x) = x
    
    nodesId = map (G.NodeId . toN . trd) nodes
    edgesId = map (\(_, _, typ, src, tgt) -> ((G.EdgeId . toN) typ, getNodeType src, getNodeType tgt)) edges
    
    graphWithNodes = foldr G.insertNode G.empty nodesId
    graphWithEdges = foldr (\(ide,src,tgt) g -> G.insertEdge ide src tgt g) graphWithNodes edgesId

lookupNodes :: [ParsedTypedNode] -> String -> String
lookupNodes nodes n = fromMaybe
                        (error ("Error getting node type of: " ++ show n))
                        (lookup n changeToListOfPairs)
  where
    changeToListOfPairs = map (\(x,_,y) -> (x,y)) nodes

instantiateRule :: ParsedTypeGraph -> RuleWithNacs -> GraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = production lhsTgm rhsTgm nacsTgm
  where
    tg = instantiateTypeGraph typeGraph
    lm = instantiateTypedGraph lhs tg
    rm = instantiateTypedGraph rhs tg
    (lhsTgm, rhsTgm) = instantiateSpan lm rm mappings
    nacsTgm = map (instantiateNac lm tg) nacs

instantiateNac :: TypedGraph a b -> G.Graph a b -> Nac -> TypedGraphMorphism a b
instantiateNac lhs tg (nacGraph, maps) = nacTgm
  where
    nacMorphism = instantiateTypedGraph nacGraph tg
    (_,nacTgm) = instantiateSpan lhs nacMorphism maps

instantiateTypedGraph :: ParsedTypedGraph -> G.Graph a b -> GraphMorphism a b
instantiateTypedGraph (_, nodes, edges) tg = gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    
    nodesG = map (toN . fstOfThree) nodes
    edgesG = map (\(id,_,_,src,tgt) -> (toN id, toN src, toN tgt)) edges
    
    nodeTyping = map (\(id,_,typ) -> (toN id, toN typ)) nodes
    edgeTyping = map (\(id,_,typ,_,_) -> (toN id, toN typ)) edges

instantiateSpan :: TypedGraph a b -> TypedGraph a b -> [Mapping] -> (TypedGraphMorphism a b, TypedGraphMorphism a b)
instantiateSpan left right mapping = (leftM, rightM)
  where
    parsedMap = map (\(t,_,s) -> (toN t, toN s)) mapping
    
    leftM = typedMorphism k left leftMap
    rightM = typedMorphism k right rightMap
    
    nodesLeft = G.nodes (domain left)
    nodesRight = G.nodes (domain right)
    
    edgesLeft = G.edges (domain left)
    edgesRight = G.edges (domain right)
    
    typegraph = codomain left
    initK = empty G.empty typegraph
    initL = empty G.empty (domain left)
    initR = empty G.empty (domain right)
    
    updateMorphisms (k,l,r) (tgt,src) =
      if nodeSrc `elem` nodesLeft && nodeTgt `elem` nodesRight
        then (newNodeK, updateNodesL, updateNodesR)
        else
          if edgeSrc `elem` edgesLeft && edgeTgt `elem` edgesRight
            then (newEdgeK, updateEdgesL, updateEdgesR)
            else (k, l, r)
      where
        nodeSrc = G.NodeId src
        nodeTgt = G.NodeId tgt
        edgeSrc = G.EdgeId src
        edgeTgt = G.EdgeId tgt
        
        nodeDom = G.insertNode nodeSrc (domain k)
        nodeType = applyNodeUnsafe left nodeSrc
        newNodeK = updateNodes nodeSrc nodeType (updateDomain nodeDom k)
        updateNodesL = updateNodes nodeSrc nodeSrc (updateDomain nodeDom l)
        updateNodesR = updateNodes nodeSrc nodeTgt (updateDomain nodeDom r)
        
        src_ e = fromMaybe (error (show e)) (G.sourceOf (domain left) e)
        tgt_ e = fromMaybe (error (show e)) (G.targetOf (domain left) e)
        edgeDom = G.insertEdge edgeSrc (src_ edgeSrc) (tgt_ edgeSrc) (domain k)
        edgeType = applyEdgeUnsafe left edgeSrc
        newEdgeK = updateEdges edgeSrc edgeType (updateDomain edgeDom k)
        updateEdgesL = updateEdges edgeSrc edgeSrc (updateDomain edgeDom l)
        updateEdgesR = updateEdges edgeSrc edgeTgt (updateDomain edgeDom r)
    
    (k, leftMap, rightMap) =
      foldl
        updateMorphisms
        (initK, initL, initR)
        parsedMap
