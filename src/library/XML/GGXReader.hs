module XML.GGXReader
  ( readGrammar
  , readGGName
  , readName
  , readNames
  , readTypeGraph
  , readRules
  , readGraphs
  , readSequences
  , readSequencesWithObjectFlow
  , instantiateRule
  , instantiateSpan
  , minimalSafetyNacsWithLog
  , showMinimalSafetyNacsLog
  ) where

import           Data.Function                (on)
import qualified Data.List                    as L
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust, fromMaybe, mapMaybe)
import           Text.XML.HXT.Core

import           Abstract.Category
import           Abstract.Constraint
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraphRule
import qualified Data.Graphs                  as G
import           Data.Graphs.Morphism         as GM
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph     as GR
import           Rewriting.DPO.TypedGraphRule
import qualified XML.Formulas                 as F
import           XML.GGXParseIn
import           XML.GGXReader.SndOrder
import           XML.GGXReader.Span
import           XML.ParsedTypes
import           XML.Utilities
import           XML.XMLUtilities

-- | Reads the grammar in the XML, adds the needed minimal safety nacs
--   to second-order, and returns the grammar and a log
readGrammar :: String -> Bool -> MorphismsConfig (TypedGraphMorphism a b)
            -> IO (Grammar (TypedGraphMorphism a b), Grammar (RuleMorphism a b), [(String, Int)])
readGrammar fileName useConstraints morphismsConf = do
  parsedTypeGraphs <- readTypeGraph fileName
  let parsedTypeGraph = case parsedTypeGraphs of
                         []    -> error "error, type graph not found"
                         ptg:_ -> ptg
  _ <- parsedTypeGraph `seq` return ()

  let typeGraph = instantiateTypeGraph parsedTypeGraph

  parsedGraphs <- readGraphs fileName
  parsedRules <- readRules fileName

  let (sndOrdRules, fstOrdRules) = L.partition (\((x,_,_,_),_) -> L.isPrefixOf "2rule_" x) parsedRules
      rulesNames = map (\((x,_,_,_),_) -> x) fstOrdRules
      productions = map (instantiateRule typeGraph) fstOrdRules

  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") (zip rulesNames productions)
  _ <- (L.null productions && error "No first-order productions were found, at least one is needed.") `seq` return ()

  parsedAtomicConstraints <- readAtomicConstraints fileName
  parsedGraphConstraints  <- readGraphConstraints fileName

  let cons
        | useConstraints =
            let namedAtomic = L.groupBy ((==) `on` fst) $ map (instantiateAtomicConstraint typeGraph) parsedAtomicConstraints
                atomic = map (joinConstraints . map snd) namedAtomic
            in instantiateConstraints parsedGraphConstraints atomic
        | otherwise = []
        where
          -- We join positive constraints with "or", which is the behaviour of AGG.
          -- We join negative constraints with "and", which is the behaviour of NACs.
          joinConstraints [x] = Atomic x
          joinConstraints (x:xs)
            | positive x = Or (Atomic x) (joinConstraints xs)
            | otherwise = And (Atomic x) (joinConstraints xs)

  -- gets only the first graph as initial, because verigraph supports only one initial graph per grammar.
  let initGraph = head (map snd parsedGraphs)
      fstOrderGrammar = grammar initGraph cons (zip rulesNames productions)

      sndOrderRules = instantiateSndOrderRules typeGraph sndOrdRules
      emptyRule = emptyGraphRule typeGraph
      sndOrderGrammar = grammar emptyRule [] sndOrderRules

      morphismsConf' = toSndOrderMorphismsConfig morphismsConf
      (sndOrderGrammarWithMinimalSafetyNacs, logNewNacs) =
        minimalSafetyNacsWithLog morphismsConf' sndOrderGrammar


  _ <- (case L.elemIndices False (map (isValid . snd) sndOrderRules) of
          []  -> []
          [a] -> error $ "Second Order Rule " ++ show a ++ " is not valid (starting from 0)."
          l   -> error $ "Second Order Rules " ++ show l ++ " are not valid (starting from 0)."
          ) `seq` return ()

  return (fstOrderGrammar, sndOrderGrammarWithMinimalSafetyNacs, logNewNacs)

readGGName :: String -> IO String
readGGName fileName = do
  name <- readName fileName
  let ret = case name of
              n:_ -> n
              _   -> "GraGra"
  return ret

-- Minimal Safety Nacs Logs

-- FIX: find a better place for this two functions
minimalSafetyNacsWithLog :: MorphismsConfig (RuleMorphism a b) -> Grammar (RuleMorphism a b)
                         -> (Grammar (RuleMorphism a b), [(String, Int)])
minimalSafetyNacsWithLog conf oldGG = (newGG, printNewNacs)
  where
    newNacs =
      map (\(n,r) ->
        let newRule = addMinimalSafetyNacs conf r
            tamNewNacs = length (nacs newRule)
            tamNacs = length (nacs r)
         in ((n, newRule), (n, tamNewNacs - tamNacs))
        ) (productions oldGG)
    newGG = oldGG {productions = map fst newNacs}
    printNewNacs = map snd newNacs


showMinimalSafetyNacsLog :: [(String, Int)] -> [String]
showMinimalSafetyNacsLog printNewNacs =
    [ "Rule " ++ r ++ ", added " ++ show n ++ " nacs" | (r,n) <- printNewNacs ]


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

readAtomicConstraints :: String -> IO[ParsedAtomicConstraint]
readAtomicConstraints fileName = runX (parseXML fileName >>> parseAtomicConstraints)

readGraphConstraints :: String -> IO[(String,F.Formula)]
readGraphConstraints fileName = runX (parseXML fileName >>> parseGraphConstraints)

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

readSequences :: Grammar (TypedGraphMorphism a b) -> String -> IO [(String, [GR.TypedGraphRule a b])]
readSequences grammar fileName = map (expandSequence grammar) <$> runX (parseXML fileName >>> parseRuleSequence)

expandSequence :: Grammar (TypedGraphMorphism a b) -> Sequence -> (String, [GR.TypedGraphRule a b])
expandSequence grammar (name,s,_) = (name, mapMaybe lookupRule . concat $ map expandSub s)
  where
    expandSub (i, s) = concat $ replicate i $ concatMap expandItens s
    expandItens (i, r) = replicate i r
    lookupRule name = L.lookup name (productions grammar)

readSequencesWithObjectFlow :: Grammar (TypedGraphMorphism a b) -> String -> IO [(String, [(String, GR.TypedGraphRule a b)], [ObjectFlow (TypedGraphMorphism a b)])]
readSequencesWithObjectFlow grammar fileName = map (prepareFlows grammar) <$> runX (parseXML fileName >>> parseRuleSequence)

prepareFlows :: Grammar (TypedGraphMorphism a b) -> Sequence -> (String, [(String, GR.TypedGraphRule a b)], [ObjectFlow (TypedGraphMorphism a b)])
prepareFlows grammar (name,s,flows) = (name, map fun getAll, objs)
  where
    fun name = (name, fromJust $ lookupRule name)
    getAll = map snd (snd $ head s) -- gets only the first subsequence
    lookupRule name = L.lookup name (productions grammar)
    objs = instantiateObjectsFlow (productions grammar) flows

instantiateObjectsFlow :: [(String, Production (TypedGraphMorphism a b))] -> [ParsedObjectFlow] -> [ObjectFlow (TypedGraphMorphism a b)]
instantiateObjectsFlow _ [] = []
instantiateObjectsFlow [] _ = []
instantiateObjectsFlow productions (o:os) =
  let
    createObject (idx,cons,prod,maps) = ObjectFlow idx prod cons (createSpan prod cons maps)
    createSpan prod cons = instantiateSpan (rightObject (searchRight prod)) (leftObject (searchLeft cons))
    searchLeft ruleName = fromJust $ L.lookup ruleName productions
    searchRight ruleName = fromJust $ L.lookup ruleName productions
  in createObject o : instantiateObjectsFlow productions os


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

instantiateAtomicConstraint :: TypeGraph a b -> ParsedAtomicConstraint -> (String, AtomicConstraint (TypedGraphMorphism a b))
instantiateAtomicConstraint tg (name, premise, conclusion, maps) = (name, buildNamedAtomicConstraint name (buildTypedGraphMorphism p c m) isPositive)
  where
    p = instantiateTypedGraph premise tg
    c = instantiateTypedGraph conclusion tg
    m = buildGraphMorphism (domain p) (domain c) (map mapToId mNodes) (map mapToId mEdges)
    isPositive = not $ L.isPrefixOf "-" name
    mapToId (a,_,b) = (toN b, toN a)
    pNodes = G.nodeIds (domain p)
    (mNodes,mEdges) = L.partition (\(_,_,x) -> G.NodeId (toN x) `elem` pNodes) maps

instantiateConstraints :: [(String, F.Formula)] -> [Constraint (TypedGraphMorphism a b)] -> [Constraint (TypedGraphMorphism a b)]
instantiateConstraints formulas atomicConstraints = map (translateFormula mappings) f
  where
    f = map snd formulas
    mappings = M.fromAscList $ zip [1..] atomicConstraints

translateFormula :: M.Map Int (Constraint (TypedGraphMorphism a b)) -> F.Formula -> Constraint (TypedGraphMorphism a b)
translateFormula m formula =
  let
    get = (m M.!) . fromIntegral
  in
    case formula of
      F.IntConst n             -> get n
      F.Not formula'           -> Not (translateFormula m formula')
      F.Or formula' formula''  -> Or (translateFormula m formula') (translateFormula m formula'')
      F.And formula' formula'' -> And (translateFormula m formula') (translateFormula m formula'')
