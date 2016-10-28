{-# LANGUAGE FlexibleContexts #-}
module XML.GGXReader
 ( readGrammar,
   readGGName,
   readName,
   readNames,
   readTypeGraph,
   readRules,
   readGraphs,
   readSequences,
   instantiateRule,
   instantiateSpan,
   minimalSafetyNacsWithLog,
   printMinimalSafetyNacsLog
   ) where

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.Valid
import qualified Data.List               as L
import           Data.Maybe              (fromMaybe, mapMaybe)
import           Data.String.Utils       (startswith)
import qualified Graph.Graph             as G
import           Graph.GraphMorphism     as GM
import           SndOrder.Rule
import           Text.XML.HXT.Core       hiding (left, right)
import           TypedGraph.Graph
import qualified TypedGraph.GraphGrammar as GG
import           TypedGraph.GraphRule    as GR
import           TypedGraph.Morphism
import           XML.GGXParseIn
import           XML.GGXSndOrderReader
import           XML.ParsedTypes
import           XML.Utilities
import           XML.XMLUtilities

type TypeGraph a b = G.Graph a b

-- | Reads the grammar in the XML, adds the needed minimal safety nacs
--   to second order, and returns the grammar and a log
readGrammar :: String -> Bool -> DPOConfig -> IO (GG.GraphGrammar a b, [(String, Int)])
readGrammar fileName useConstraints dpoConfig = do
  parsedTypeGraphs <- readTypeGraph fileName
  let parsedTypeGraph = case parsedTypeGraphs of
                         []    -> error "error, type graph not found"
                         ptg:_ -> ptg
  _ <- parsedTypeGraph `seq` return ()

  --let a tg g = instantiateTypedGraph g tg
  let typeGraph = instantiateTypeGraph parsedTypeGraph
  --graphs <- readGraphs' fileName

  --let initialGraphs = map (a typeGraph) $ concat graphs

  parsedRules <- readRules fileName

  let (sndOrdRules, fstOrdRules) = L.partition (\((x,_,_,_),_) -> startswith "2rule_" x) parsedRules
      rulesNames = map (\((x,_,_,_),_) -> x) fstOrdRules
      rules = map (instantiateRule typeGraph) fstOrdRules

  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") (zip rulesNames rules)
  _ <- (L.null rules && error "No first order rules were found, at least one is needed.") `seq` return ()

  parsedConstraints <- readConstraints fileName

  let cons = if useConstraints then
               map (instantiateAtomicConstraint typeGraph) parsedConstraints
             else []

  --print "Validity"
  --print $ testeCons initialGraphs cons

  let initGraph = GM.empty typeGraph typeGraph
      sndOrderRules = instantiateSndOrderRules typeGraph sndOrdRules
      gg = GG.graphGrammar initGraph cons (zip rulesNames rules) sndOrderRules

  _ <- (case L.elemIndices False (map (isValid . snd) sndOrderRules) of
          []  -> []
          [a] -> error $ "Second Order Rule " ++ show a ++ " is not valid (starting from 0)."
          l   -> error $ "Second Order Rules " ++ show l ++ " are not valid (starting from 0)."
          ) `seq` return ()

  return $ minimalSafetyNacsWithLog dpoConfig gg

--testeCons :: [TypedGraph a b] -> [AtomicConstraint (TypedGraphMorphism a b)] -> [[Bool]]
--testeCons [] _      = []
--testeCons (g:gs) cs = satisfiesCons g cs : testeCons gs cs
--  where
--    satisfiesCons g cs = map (satisfiesAtomicConstraint g) cs

readGGName :: String -> IO String
readGGName fileName = do
  name <- readName fileName
  let ret = case name of
              n:_ -> n
              _   -> "GraGra"
  return ret

-- Minimal Safety Nacs Logs

-- FIX: find a better place for this two functions
minimalSafetyNacsWithLog :: DPOConfig -> GG.GraphGrammar a b -> (GG.GraphGrammar a b, [(String, Int)])
minimalSafetyNacsWithLog conf oldGG = (newGG, printNewNacs)
  where
    newNacs =
      map (\(n,r) ->
        let newRule = addMinimalSafetyNacs conf r
            tamNewNacs = length (getNACs newRule)
            tamNacs = length (getNACs r)
         in ((n, newRule), (n, tamNewNacs - tamNacs))
        ) (GG.sndOrderRules oldGG)
    newGG = oldGG {GG.sndOrderRules = map fst newNacs}
    printNewNacs = map snd newNacs

printMinimalSafetyNacsLog :: [(String, Int)] -> [String]
printMinimalSafetyNacsLog printNewNacs =
    ["Adding minimal safety nacs to second order rules:"]
    ++ map (\(r,n) -> "Rule " ++ r ++ ", added " ++ show n ++ " nacs") printNewNacs
    ++ ["All minimal safety nacs added!"]


-- | Reads the names of node/edge types and NACs, which are necessary when reexporting this grammar.
--
-- To lookup the name of a node type, use @"I" ++ show nodeId@ as key, where @nodeId@ is the ID of
-- the node in the type graph. Lookup of edge types is analogous.
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

readConstraints :: String -> IO[ParsedAtomicConstraint]
readConstraints fileName = runX (parseXML fileName >>> parseAtomicConstraints)

--readGraphs' :: String -> IO[[ParsedTypedGraph]]
--readGraphs' fileName = runX (parseXML fileName >>> parseGraphs)

readGraphs :: String -> IO [(String, TypedGraph a b)]
readGraphs fileName =
  do
    [parsedTypeGraph] <- readTypeGraph fileName
    let typeGraph = instantiateTypeGraph parsedTypeGraph

    [parsedGraphs] <- runX (parseXML fileName >>> parseGraphs)
    let instantiate graph@(name, _, _) = (name, instantiateTypedGraph graph typeGraph)

    return $ map instantiate parsedGraphs

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

instantiateTypeGraph :: ParsedTypeGraph -> TypeGraph a b
instantiateTypeGraph (nodes, edges) = graphWithEdges
  where
    getNodeType = G.NodeId . toN . lookupNodes nodes
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

instantiateRule :: TypeGraph a b -> RuleWithNacs -> GraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = buildProduction lhsTgm rhsTgm nacsTgm
  where
    lm = instantiateTypedGraph lhs typeGraph
    rm = instantiateTypedGraph rhs typeGraph
    (lhsTgm, rhsTgm) = instantiateSpan lm rm mappings
    nacsTgm = map (instantiateNac lm typeGraph) nacs

instantiateNac :: TypedGraph a b -> G.Graph a b -> Nac -> TypedGraphMorphism a b
instantiateNac lhs tg (nacGraph, maps) = nacTgm
  where
    nacMorphism = instantiateTypedGraph nacGraph tg
    (_,nacTgm) = instantiateSpan lhs nacMorphism maps

instantiateAtomicConstraint :: TypeGraph a b -> ParsedAtomicConstraint -> AtomicConstraint (TypedGraphMorphism a b)
instantiateAtomicConstraint tg (name, premise, conclusion, maps) = buildNamedAtomicConstraint name (buildTypedGraphMorphism p c m) isPositive
  where
    p = instantiateTypedGraph premise tg
    c = instantiateTypedGraph conclusion tg
    m = buildGraphMorphism (domain p) (domain c) (map mapToId mNodes) (map mapToId mEdges)
    isPositive = not $ startswith "-" name
    mapToId (a,_,b) = (toN b, toN a)
    pNodes = G.nodes (domain p)
    (mNodes,mEdges) = L.partition (\(_,_,x) -> G.NodeId (toN x) `elem` pNodes) maps


instantiateTypedGraph :: ParsedTypedGraph -> TypeGraph a b -> GraphMorphism a b
instantiateTypedGraph (_, nodes, edges) tg = buildGraphMorphism g tg nodeTyping edgeTyping
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

    leftM = buildTypedGraphMorphism k left leftMap
    rightM = buildTypedGraphMorphism k right rightMap

    nodesLeft = G.nodes (domain left)
    nodesRight = G.nodes (domain right)

    edgesLeft = G.edges (domain left)
    edgesRight = G.edges (domain right)

    typegraph = codomain left
    initK = empty G.empty typegraph
    initL = empty G.empty (domain left)
    initR = empty G.empty (domain right)

    updateMorphisms (k,l,r) (tgt,src)
      | nodeSrc `elem` nodesLeft && nodeTgt `elem` nodesRight = (newNodeK, updateNodesL, updateNodesR)
      | edgeSrc `elem` edgesLeft && edgeTgt `elem` edgesRight = (newEdgeK, updateEdgesL, updateEdgesR)
      | otherwise = (k, l, r)
      where nodeSrc = G.NodeId src
            nodeTgt = G.NodeId tgt
            edgeSrc = G.EdgeId src
            edgeTgt = G.EdgeId tgt
            nodeDom = G.insertNode nodeSrc (domain k)
            nodeType = extractNodeType left nodeSrc
            newNodeK = updateNodes nodeSrc nodeType (updateDomain nodeDom k)
            updateNodesL = updateNodes nodeSrc nodeSrc (updateDomain nodeDom l)
            updateNodesR = updateNodes nodeSrc nodeTgt (updateDomain nodeDom r)
            src_ e = fromMaybe (error (show e)) (G.sourceOf (domain left) e)
            tgt_ e = fromMaybe (error (show e)) (G.targetOf (domain left) e)
            edgeDom
              = G.insertEdge edgeSrc (src_ edgeSrc) (tgt_ edgeSrc) (domain k)
            edgeType = extractEdgeType left edgeSrc
            newEdgeK = updateEdges edgeSrc edgeType (updateDomain edgeDom k)
            updateEdgesL = updateEdges edgeSrc edgeSrc (updateDomain edgeDom l)
            updateEdgesR = updateEdges edgeSrc edgeTgt (updateDomain edgeDom r)

    (k, leftMap, rightMap) =
      foldl
        updateMorphisms
        (initK, initL, initR)
        parsedMap
