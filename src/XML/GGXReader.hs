module XML.GGXReader
 ( readGrammar,
   readGGName,
   readName,
   readNames,
   readTypeGraph,
   readRules,
   readSequences,
   instantiateRule
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
import           Graph.RuleMorphism
import           Graph.TypedGraphMorphism
import           Text.Read                (readMaybe)
import           Text.XML.HXT.Core
import           XML.GGXParseIn
import           XML.ParsedTypes
import qualified XML.ParseSndOrderRule    as SO
import           XML.XMLUtilities

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

  let typeGraph = codomain . domain . GR.left $ head rules
      initGraph = GM.empty typeGraph typeGraph
      a = SO.parseSndOrderRules sndOrdRules
      c = map (instantiateSndOrderRule parsedTypeGraph) a
      d = map (\(_,morphs) -> ((uncurry production) morphs) []) c
      sndOrderNames = map fst c
  
  return $ GG.graphGrammar initGraph (zip rulesNames rules) (zip sndOrderNames d)

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

instantiateRule :: ParsedTypeGraph -> RuleWithNacs -> GraphRule a b
instantiateRule typeGraph ((_, lhs, rhs, mappings), nacs) = graphRule lhsTgm rhsTgm nacsTgm
  where
    tg = instantiateTypeGraph typeGraph
    lm = instantiateTypedGraph lhs tg
    rm = instantiateTypedGraph rhs tg
    km = instantiateInterface mappings lm
    nacsTgm = map (instantiateNac lm tg) nacs
    rhsTgm = instantiateTgm km rm mappings
    lhsTgm = instantiateLeft km lm

instantiateNac ::  GraphMorphism a b -> G.Graph a b -> Nac -> TypedGraphMorphism a b
instantiateNac lhs graph (nacG, maps) = nacTgm
  where
    nacMorphism = instantiateTypedGraph nacG graph
    nacTgm = instantiateTgm lhs nacMorphism maps

instantiateTypeGraph :: ParsedTypeGraph -> G.Graph a b
instantiateTypeGraph (nodes, edges) = graphWithEdges
  where
    getNodeId y = G.NodeId (read y :: Int)
    getEdgeId y = G.EdgeId (read y :: Int)
    getNodeType n = G.NodeId (read (lookupNodes n nodes) :: Int)
    nodesId = map (\(_,_,y) -> getNodeId y) nodes
    graphWithNodes = foldr G.insertNode G.empty nodesId
    edgesId = map (\(_, _, typ, src, tgt) -> (getEdgeId typ, getNodeType src, getNodeType tgt)) edges
    graphWithEdges = foldr (\(ide,src,tgt) g -> G.insertEdge ide src tgt g) graphWithNodes edgesId

lookupNodes :: String -> [ParsedTypedNode] -> String
lookupNodes n nodes = fromMaybe (error ("Error getting node type of: " ++ show n)) (lookup n changeToListOfPairs)
  where
    changeToListOfPairs = map (\(x,_,y) -> (x,y)) nodes

instantiateTypedGraph :: ParsedTypedGraph -> G.Graph a b -> GraphMorphism a b
instantiateTypedGraph (_,b,c) tg = gmbuild g tg nodeTyping edgeTyping
  where
    g = G.build nodesG edgesG
    nodesG = map (toN . fstOfThree) b
    edgesG = map (\(x,_,_,y,z) -> (toN x, toN y, toN z)) c
    nodeTyping = map (\(x,_,y) -> (toN x, toN y)) b
    edgeTyping = map (\(x,_,y,_,_) -> (toN x, toN y)) c

getSrc :: G.Graph a b -> G.EdgeId -> G.NodeId
getSrc g e = fromMaybe (error "Error, graph with source edges function non total") $ G.sourceOf g e

getTgt :: G.Graph a b -> G.EdgeId -> G.NodeId
getTgt g e = fromMaybe (error "Error, graph with target edges function non total") $ G.targetOf g e

applyNodeTotalMap :: GraphMorphism a b -> G.NodeId -> G.NodeId
applyNodeTotalMap m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNode m n

applyEdgeTotalMap :: GraphMorphism a b -> G.EdgeId -> G.EdgeId
applyEdgeTotalMap m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdge m e

fstOfThree :: (t1, t2, t3) -> t1
fstOfThree (x,_,_) = x

thirdOfThree :: (t1, t2, t3) -> t3
thirdOfThree (_,_,x) = x

instantiateInterface :: [Mapping] -> GraphMorphism a b -> GraphMorphism a b
instantiateInterface mapping l = gmbuild graphK (codomain l) nodeType edgeType
  where
    listMap = map (toN . thirdOfThree) mapping
    nodesK = filter (\(G.NodeId x) -> x `elem` listMap) (G.nodes (domain l))
    nodesK2 = map (\(G.NodeId x) -> x) nodesK
    edgesK = filter (\(G.EdgeId x) -> x `elem` listMap) (G.edges (domain l))
    src = getSrc (domain l)
    src2 (G.NodeId e) = e
    tgt = getTgt (domain l)
    tgt2 (G.NodeId e) = e
    edgesK2 = map (\edg@(G.EdgeId x) -> (x, src2 (src edg), tgt2 (tgt edg))) edgesK

    graphK = G.build nodesK2 edgesK2
    nodeTyping = map (\n -> (n, applyNodeTotalMap l n)) nodesK
    edgeTyping = map (\e -> (e, applyEdgeTotalMap l e)) edgesK
    nodeType = map (\(G.NodeId x, G.NodeId y) -> (x,y)) nodeTyping
    edgeType = map (\(G.EdgeId x, G.EdgeId y) -> (x,y)) edgeTyping

instantiateLeft :: GraphMorphism a b -> GraphMorphism a b -> TypedGraphMorphism a b
instantiateLeft k l = typedMorphism k l edges
  where
    ini = empty (domain k) (domain l)
    nodes = foldr (\n -> updateNodes n n) ini (G.nodes (domain k))
    edges = foldr (\e -> updateEdges e e) nodes (G.edges (domain k))

instantiateDownK :: GraphMorphism a b -> GraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
instantiateDownK k r maps = typedMorphism k r edges
  where
    ini = empty (domain k) (domain r)
    nodes = foldr (\n -> updateNodes n (applyNodeTotalTGMMap maps n)) ini (G.nodes (domain k))
    edges = foldr (\e -> updateEdges e (applyEdgeTotalTGMMap maps e)) nodes (G.edges (domain k))

applyNodeTotalTGMMap :: TypedGraphMorphism a b -> G.NodeId -> G.NodeId
applyNodeTotalTGMMap m n = fromMaybe (error "Error, apply node in a non total morphism") $ applyNodeTGM m n

applyEdgeTotalTGMMap :: TypedGraphMorphism a b -> G.EdgeId -> G.EdgeId
applyEdgeTotalTGMMap m e = fromMaybe (error "Error, apply edge in a non total morphism") $ applyEdgeTGM m e

instantiateTgm :: GraphMorphism a b -> GraphMorphism a b -> [Mapping] -> TypedGraphMorphism a b
instantiateTgm s t maps = typedMorphism s t gmMap
  where
    graphS = domain s
    initGm = empty graphS (domain t)
    listNodes = map (\(G.NodeId x) -> x) (G.nodes graphS)
    listInt = map (\(x,_,y) -> (toN x, toN y)) maps
    gmMap = foldr (\(imag,orig) gm ->
      if orig `elem` listNodes
         then updateNodes (G.NodeId orig) (G.NodeId imag) gm
         else updateEdges (G.EdgeId orig) (G.EdgeId imag) gm)
      initGm listInt

instantiateSndOrderRule :: ParsedTypeGraph -> (SndOrderRuleSide, SndOrderRuleSide) -> (String,(RuleMorphism a b, RuleMorphism a b))
instantiateSndOrderRule typegraph (l@(_,nameL,leftL),r@(_,_,rightR)) = (nameL, instantiateRuleMorphisms (l,ruleLeft) (r,ruleRight))
  where
    ruleLeft = instantiateRule typegraph (leftL,[])
    ruleRight = instantiateRule typegraph (rightR,[])

instantiateRuleMorphisms :: (SndOrderRuleSide, GraphRule a b)
                         -> (SndOrderRuleSide, GraphRule a b)
                         -> (RuleMorphism a b , RuleMorphism a b)
instantiateRuleMorphisms (parsedLeft, left) (parsedRight, right) =
  (ruleMorphism ruleK left leftKtoLeftL interfaceKtoL rightKtoRightL,
   ruleMorphism ruleK right leftKtoLeftR interfaceKtoR rightKtoRightR)
    where
      ruleK = graphRule leftK rightK []
      leftK = typedMorphism graphKruleK graphLruleK (mapping interfaceKtoL)
      rightK = typedMorphism graphKruleK graphRruleK (mapping interfaceKtoR)
      graphKruleL = domain (GR.left left)
      graphKruleR = domain (GR.left right)
      interfaceKtoR = instantiateDownK graphKruleK (codomain (GR.right left)) (GR.right left)
      
      (graphLruleK, leftKtoLeftL, leftKtoLeftR) =
        instantiateObjectName
          (codomain (GR.left left))
          (codomain (GR.left right))
          (SO.getLeftObjNameMapping parsedLeft parsedRight)
      
      (graphRruleK, rightKtoRightL, rightKtoRightR) = 
        instantiateObjectName
          (codomain (GR.right left))
          (codomain (GR.right right))
          (SO.getRightObjNameMapping parsedLeft parsedRight)
      
      (graphKruleK, {-mappingLeftK, mappingRightK-}_, _, interfaceKtoL) =
        instantiateKSndOrder
          (SO.getLeftObjNameMapping parsedLeft parsedRight)
          graphKruleL graphKruleR

instantiateObjectName :: TypedGraph a b -> TypedGraph a b -> [Mapping]
                      -> (TypedGraph a b, TypedGraphMorphism a b, TypedGraphMorphism a b)
instantiateObjectName left right mapping = (interfaceL, leftLtoL, leftLtoR)
  where
    interfaceL = instantiateInterface mapping left
    leftLtoL = instantiateLeft interfaceL left
    leftLtoR = instantiateTgm interfaceL right mapping

instantiateKSndOrder :: [Mapping] -> TypedGraph a b -> TypedGraph a b
                     -> (TypedGraph a b, TypedGraphMorphism a b, TypedGraphMorphism a b, TypedGraphMorphism a b)
instantiateKSndOrder ruleMap left right = (graphK, leftK, rightK, upK)
  where
    graphK = instantiateInterface ruleMap left
    leftK = instantiateLeft graphK left
    rightK = instantiateTgm graphK right ruleMap
    upK = instantiateLeft left graphK

toN :: String -> Int
toN x = case readMaybe x :: Maybe Int of
          Just n -> n
          Nothing -> error $ "Error converting id (" ++ x ++ ") to number"
