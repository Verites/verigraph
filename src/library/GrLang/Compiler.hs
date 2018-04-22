{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module GrLang.Compiler
  ( compileFile
  , compile
  , compileDecl
  , compileGraph
  , compileMorphism
  , compileRule
  ) where

import           Control.Monad.Except           (ExceptT (..), mapExceptT)
import           Control.Monad.State
import           Data.Either                    (lefts, rights)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Lazy.IO              as Text
import           Data.Text.Prettyprint.Doc      (Pretty (..), (<>))
import qualified Data.Text.Prettyprint.Doc      as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP
import           System.FilePath                (takeDirectory, (</>))
import           System.IO.Error                (ioeGetErrorString, tryIOError)

import           Base.Annotation                (Annotated (..), Located)
import qualified Base.Annotation                as Ann
import           Base.Location
import           Data.TypedGraph                (Edge (..), EdgeId, Node (..), NodeId)
import qualified Data.TypedGraph                as TGraph
import qualified Data.TypedGraph.Morphism       as TGraph
import           GrLang.AST
import           GrLang.Monad
import           GrLang.Parser
import           GrLang.Value
import           Rewriting.DPO.TypedGraph       (Production (..))
import qualified Util.Map                       as Map
import           Util.Monad

compileFile :: (MonadIO m, MonadGrLang m) => FilePath -> ExceptT Error m ()
compileFile path = importIfNeeded_ path $
  loadModule (A Nothing path) >>= compile

compile :: (MonadIO m, MonadGrLang m) => [TopLevelDeclaration] -> ExceptT Error m ()
compile = mapMCollectErrors_ compileDecl

compileDecl :: (MonadIO m, MonadGrLang m) => TopLevelDeclaration -> ExceptT Error m ()
compileDecl (DeclNodeType n) = addNodeType n
compileDecl (DeclEdgeType e s t) = addEdgeType e s t
compileDecl (DeclGraph name graphDecls) =
  putValue name . VGraph =<< compileGraph graphDecls
compileDecl (DeclMorphism name dom cod morphDecls) =
  putValue name . VMorph =<< compileMorphism' (Ann.locationOf name) dom cod morphDecls
compileDecl (DeclRule name ruleDecls) =
  putValue name . VRule =<< compileRule ruleDecls
compileDecl (Import (A loc path)) = do
  -- Interpret the imported path relative to the directory of the current path
  let path' = case loc of
        Nothing -> path
        Just (Location currPath _) -> takeDirectory currPath </> path
  importIfNeeded_ path' $
    loadModule (A loc path') >>= compile

loadModule :: (MonadIO m, MonadGrLang m) => Located FilePath -> ExceptT Error m [TopLevelDeclaration]
loadModule (A loc path) = do
  textOrError <- liftIO . tryIOError $ Text.readFile path
  case textOrError of
    Right text -> parseModule path text
    Left ioError -> throwError loc . PP.fillSep $
      PP.words "Error reading file" ++ [ PP.dquotes (pretty path) <> PP.colon, pretty (ioeGetErrorString ioError) ]

type GrElem = Either (GrNode, NodeType) (GrEdge, EdgeType)

data GraphState = GrSt
  { grNamedElements  :: Map Text GrElem
  , grAnonymousEdges :: [(GrEdge, EdgeType)]
  , grFreeNodeId     :: NodeId
  , grFreeEdgeId     :: EdgeId
  }

grAllEdges :: GraphState -> [(GrEdge, EdgeType)]
grAllEdges state = (rights . Map.elems) (grNamedElements state) ++ grAnonymousEdges state

type GraphT m a = ExceptT Error (StateT GraphState m) a

evalGraphT :: MonadGrLang m => GraphState -> GraphT m a -> ExceptT Error m a
evalGraphT state = mapExceptT (`evalStateT` state)

emptyGraphState :: GraphState
emptyGraphState = GrSt Map.empty [] 0 0

getNode :: MonadGrLang m => Located Text -> GraphT m (GrNode, NodeType)
getNode (A loc name) = do
  existingElem <- gets (Map.lookup name . grNamedElements)
  let existingNode = getLeft =<< existingElem
  getOrError loc "node" name (pure existingNode) (nodeLocation . fst)
  where getLeft = either Just (const Nothing)

getElem :: MonadGrLang m => Located Text -> GraphT m GrElem
getElem (A loc name) = do
  existingElem <- gets (Map.lookup name . grNamedElements)
  getOrError loc "node or edge" name (pure existingElem) (either (nodeLocation . fst) (edgeLocation . fst))

assembleGraph :: MonadGrLang m => GraphT m GrGraph
assembleGraph = do
  tgraph <- getTypeGraph
  (nodes, edges) <- gets (Map.partitionEithers . grNamedElements)
  anonEdges <- gets grAnonymousEdges
  return $ TGraph.fromNodesAndEdges tgraph
    [ (node, nodeId ntype) | (node, ntype) <- Map.elems nodes ]
    [ (edge, edgeId etype) | (edge, etype) <- Map.elems edges ++ anonEdges ]

compileGraph :: MonadGrLang m => [GraphDeclaration] -> ExceptT Error m GrGraph
compileGraph = mapExceptT (`evalStateT` emptyGraphState) . compileGraph'

compileGraph' :: MonadGrLang m => [GraphDeclaration] -> GraphT m GrGraph
compileGraph' decls = mapMCollectErrors compileGraphDecl decls >> assembleGraph

compileGraphDecl :: MonadGrLang m => GraphDeclaration -> GraphT m ()
compileGraphDecl (DeclNodes nodes typeName) = do
  nodeType <- getNodeType typeName
  mapMCollectErrors_ (createNode nodeType) nodes

compileGraphDecl (DeclEdges srcName edges tgtName) = do
  (src, srcType) <- getNode srcName
  (tgt, tgtType) <- getNode tgtName
  forMCollectErrors_ edges $ \(edges', typeName) -> do
    edgeType <- getEdgeType typeName srcType tgtType
    case edges' of
      AnonymousEdge ->
        () <$ createEdge edgeType (nodeId src) (nodeId tgt) (Ann.locationOf typeName) Nothing
      NamedEdges names -> forMCollectErrors_ names $ \(A loc name) ->
        createEdge edgeType (nodeId src) (nodeId tgt) loc (Just name)

createNode :: MonadGrLang m => NodeType -> Located Text -> GraphT m NodeId
createNode nodeType (A loc name) = do
  prevNode <- gets (Map.lookup name . grNamedElements)
  case prevNode of
    Just (Left (n, _)) -> reportAlreadyDefined loc "Node" name (nodeLocation n)
    Just (Right (e, _)) -> reportAlreadyDefined loc "An edge" name (edgeLocation e)
    Nothing -> do
      newId <- gets grFreeNodeId
      let node = Node newId (Just (Metadata (Just name) loc))
      modify $ \state -> state
        { grNamedElements = Map.insert name (Left (node, nodeType)) (grNamedElements state)
        , grFreeNodeId = newId + 1 }
      return newId

createEdge :: Monad m => EdgeType -> NodeId -> NodeId -> Maybe Location -> Maybe Text -> GraphT m EdgeId
createEdge edgeType src tgt loc (Just name) = do
  prevEdge <- gets (Map.lookup name . grNamedElements)
  case prevEdge of
    Just (Right (e,_)) -> reportAlreadyDefined loc "Edge" name (edgeLocation e)
    Just (Left (n,_)) -> reportAlreadyDefined loc "A node" name (nodeLocation n)
    Nothing -> do
      newId <- gets grFreeEdgeId
      let edge = Edge newId src tgt (Just $ Metadata (Just name) loc)
      modify $ \state -> state
        { grNamedElements = Map.insert name (Right (edge, edgeType)) (grNamedElements state)
        , grFreeEdgeId = newId + 1 }
      return newId

createEdge edgeType src tgt loc Nothing = do
  newId <- gets grFreeEdgeId
  let edge = Edge newId src tgt (Just $ Metadata Nothing loc)
  modify $ \state -> state
    { grAnonymousEdges = (edge, edgeType) : grAnonymousEdges state
    , grFreeEdgeId = newId + 1 }
  return newId


compileMorphism' :: MonadGrLang m => Maybe Location -> Located Text -> Located Text -> [MorphismDeclaration] -> ExceptT Error m GrMorphism
compileMorphism' loc domName codName decls = do
  [domain, codomain] <- mapMCollectErrors getGraph [(domName, "domain"), (codName, "codomain")]
  compileMorphism loc domain codomain decls
  where
    getGraph (name, descr) = do
      val <- getValue name
      case val of
        VGraph g -> return g
        (VMorph _) -> throwError loc . PP.fillSep . PP.words $ "Cannot use a morphism as " <> descr <> "."
        (VRule _) -> throwError loc . PP.fillSep . PP.words $ "Cannot use a rule as " <> descr <> "."

compileMorphism :: MonadGrLang m => Maybe Location -> GrGraph -> GrGraph -> [MorphismDeclaration] -> ExceptT Error m GrMorphism
compileMorphism loc domain codomain decls = do
  let domElems = namedElementsOf domain
      codElems = namedElementsOf codomain
  mappings <- mapMCollectErrors (compileMapping domElems codElems) decls

  let nodeMapping = Map.fromList [ (nodeId domNode, nodeId codNode) | (_, domNode, codNode) <- concat (lefts mappings) ]
  let edgeMapping' = concat (rights mappings)
  mapMCollectErrors_ (validateEdgeMapping nodeMapping) edgeMapping'
  let edgeMapping = Map.fromList [ (edgeId domEdge, edgeId codEdge) | (_, domEdge, codEdge) <- edgeMapping' ]

  let missingNodes = Set.difference (Set.fromList $ TGraph.nodeIds domain) (Map.keysSet nodeMapping)
      missingEdges = Set.difference (Set.fromList $ TGraph.edgeIds domain) (Map.keysSet edgeMapping)
  unless (Set.null missingNodes && Set.null missingEdges) $ do
    let missingMsg ids kind getName
          | Set.null ids = []
          | otherwise = [PP.fillSep $ kind : map (maybe "<missing name>" pretty . getName) (Set.toList ids)]
    throwError loc . PP.fillSep $
      PP.words "The morphism is not total: missing mappings for" ++
      PP.punctuate "; "
        ( missingMsg missingNodes "nodes" (fmap nodeName . (`TGraph.lookupNode` domain))
        ++ missingMsg missingEdges "edges" (fmap edgeName . (`TGraph.lookupEdge` domain)) )

  return $ TGraph.fromGraphsAndLists domain codomain (Map.toList nodeMapping) (Map.toList edgeMapping)

  where
    validateEdgeMapping nodeMapping (loc, domEdge, codEdge) = do
      unless (Map.lookup (sourceId domEdge) nodeMapping == Just (sourceId codEdge)) . throwError loc . PP.fillSep $
        PP.words "Source mismatch when mapping edge."
      unless (Map.lookup (targetId domEdge) nodeMapping == Just (targetId codEdge)) . throwError loc . PP.fillSep $
        PP.words "Target mismatch when mapping edge."

    compileMapping domElems codElems (DeclMapping domNames (A codLoc codName)) = do
      let maybeCodElem = Map.lookup codName codElems
      codElem <- getOrError codLoc "node or edge" codName (pure maybeCodElem) (either (nodeLocation . fst) (edgeLocation . fst))
      case codElem of
          Left (codNode, codType) -> fmap Left . forMCollectErrors domNames $ \(A domLoc domName) -> do
            let maybeDomNode = maybeLeft =<< Map.lookup domName domElems
            (domNode, domType) <- getOrError domLoc "node" domName (pure maybeDomNode) (nodeLocation . fst)
            unless (domType == codType) . throwError domLoc . PP.fillSep $
              PP.words "Type mismatch when mapping node."
            return (domLoc, domNode, codNode)
          Right (codEdge, codType) -> fmap Right . forMCollectErrors domNames $ \(A domLoc domName) -> do
            let maybeDomEdge = maybeRight =<< Map.lookup domName domElems
            (domEdge, domType) <- getOrError domLoc "edge" domName (pure maybeDomEdge) (edgeLocation . fst)
            unless (domType == codType) . throwError domLoc . PP.fillSep $
              PP.words "Type mismatch when mapping edge."
            return (domLoc, domEdge, codEdge)

namedElementsOf :: GrGraph -> Map Text (Either (GrNode, NodeId) (GrEdge, EdgeId))
namedElementsOf graph = Map.fromList $ namedNodes ++ namedEdges
  where
    namedNodes = [ (nodeName node, Left (node, ntype)) | (node, ntype) <- TGraph.nodes graph]
    namedEdges = [ (edgeName edge, Right (edge, etype)) | (edge, etype) <- TGraph.edges graph ]

maybeLeft :: Either a b -> Maybe a
maybeLeft = either Just (const Nothing)

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

compileRule :: MonadGrLang m => [RuleDeclaration] -> ExceptT Error m GrRule
compileRule decls = do
  -- Compile the LHS graph from match declarations
  lhsState <- evalGraphT emptyGraphState $ mapMCollectErrors_ compileMatch decls >> get
  lhsGraph <- evalGraphT lhsState assembleGraph

  -- Add clones and remove deleted elements to interface graph
  (clonedMaps, interfaceState) <- evalGraphT lhsState $ do
    mapMCollectErrors_ compileDelete decls
    clonedMaps <- assembleMaps <$> mapMCollectErrors compileClone decls
    (,) clonedMaps <$> get
  interfaceGraph <- evalGraphT interfaceState assembleGraph
  let lhsMorph = completeMorphismWithInclusions interfaceGraph lhsGraph clonedMaps

  -- Add created elements to the RHS graph
  (joinedMaps, rhsGraph) <- evalGraphT interfaceState $ do
    joinedMaps@(joinedNodeMap, _) <- assembleMaps <$> mapMCollectErrors compileJoin decls
    unless (Map.null joinedNodeMap) . modify $ \state ->
      -- Correct edges that are incident to joined nodes
      let adjustEdge (Edge e s t info, etype) =
            (Edge e (Map.findWithDefault s s joinedNodeMap) (Map.findWithDefault t t joinedNodeMap) info, etype)
      in state
      { grNamedElements = Map.map (fmap adjustEdge) (grNamedElements state)
      , grAnonymousEdges = map adjustEdge (grAnonymousEdges state)
      }
    mapMCollectErrors_ compileCreate decls
    (,) joinedMaps <$> assembleGraph
  let rhsMorph = completeMorphismWithInclusions interfaceGraph rhsGraph joinedMaps

  theNacs <- catMaybes <$> mapMCollectErrors (compileForbid lhsGraph lhsState) decls
  return (Production lhsMorph rhsMorph theNacs)
  where
    assembleMaps :: (Ord n, Ord e) => [Either [(n, n)] [(e, e)]] -> (Map n n, Map e e)
    assembleMaps mixedMappings =
      ( Map.fromList . concat $ lefts mixedMappings, Map.fromList . concat $ rights mixedMappings )

    -- Given partial components of a graph morphism, complete the morphism as if it were an inclusion
    completeMorphismWithInclusions :: GrGraph -> GrGraph -> (Map NodeId NodeId, Map EdgeId EdgeId) -> GrMorphism
    completeMorphismWithInclusions dom cod (partialNodeMap, partialEdgeMap) =
      let insertIfNew x = Map.insertWith (\_ old -> old) x x
          nodeMap = foldr insertIfNew partialNodeMap (TGraph.nodeIds dom)
          edgeMap = foldr insertIfNew partialEdgeMap (TGraph.edgeIds dom)
      in TGraph.fromGraphsAndLists dom cod (Map.toList nodeMap) (Map.toList edgeMap)

-- | Given a match declaration, add all declared elements to the state.
compileMatch :: MonadGrLang m => RuleDeclaration -> GraphT m ()
compileMatch (DeclMatch decls) = mapMCollectErrors_ compileGraphDecl decls
compileMatch _                 = return ()

-- | Given a clone declaration, add the appropriate duplicate elements to the state
-- and return a list of pairs @(originalId,clonedId)@ under Left for nodes and Right for edges.
--
-- NOTE: Incident edges are not cloned along with nodes. This is the intended behaviour for
-- GrLang: if a matched edge should be cloned, this should be done explicitly.
--compileClone :: MonadGrLang m => RuleDeclaration -> GraphT m [Either (NodeId, NodeId) (EdgeId, EdgeId)]
compileClone :: MonadGrLang m => RuleDeclaration -> GraphT m (Either [(NodeId, NodeId)] [(EdgeId, EdgeId)])
compileClone (DeclClone name clones) = do
  elem <- getElem name
  case elem of
    Left (node, typ) -> fmap Left . forMCollectErrors clones $ \cloneName -> do
      cloneId <- createNode typ cloneName
      return (cloneId, nodeId node)
    Right (edge, typ) -> fmap Right . forMCollectErrors clones $ \(A loc cloneName) -> do
      cloneId <- createEdge typ (sourceId edge) (targetId edge) loc (Just cloneName)
      return (cloneId, edgeId edge)
compileClone _ = return $ Left []

-- | Given a delete declaration, remove all declared elements from the state.
-- If a deleted node has incident edges, the behaviour depends on the mode:
-- in Isolated mode, compilation fails; in WithMatchedEdges mode, incident edges
-- are removed from the state as well.
compileDelete :: MonadGrLang m => RuleDeclaration -> GraphT m ()
compileDelete (DeclDelete deleted mode) = forMCollectErrors_ deleted $ \(A loc name) -> do
  elem <- getElem (A loc name)
  case (elem, mode) of
    (Left (node, _), Isolated) -> do
      incidentEdges <- gets $ filter (isIncidentTo node) . grAllEdges
      unless (null incidentEdges) . throwError loc . PP.fillSep $
          PP.words "Cannot delete node" ++ pretty name : PP.words "without deleting its incident edges: "
          ++ PP.punctuate ", " (map (pretty . edgeName . fst) incidentEdges)
    (Left (node, _), WithMatchedEdges) ->
      modify $ \state -> state
        { grNamedElements = Map.filter (either (const True) (not . isIncidentTo node)) (grNamedElements state)
        , grAnonymousEdges = filter (not . isIncidentTo node) (grAnonymousEdges state)
        }
    (Right _, _) -> return ()
  modify $ \state -> state
    { grNamedElements = Map.delete name (grNamedElements state) }
  where isIncidentTo node (edge, _) = sourceId edge == nodeId node || targetId edge == nodeId node
compileDelete _ = return ()

-- | Given a create declaration, add all declared elements to the state.
compileCreate :: MonadGrLang m => RuleDeclaration -> GraphT m ()
compileCreate (DeclCreate decls) = mapMCollectErrors_ compileGraphDecl decls
compileCreate _                  = return ()

-- | Given a join declaration, remove the appropriate duplicate elements from the state,
-- create a joint element with the appropriate name and return a list of pairs @(originalId, jointId)@
-- under Left for nodes and Right for edges.
--
-- NOTE: when nodes are joined, any incident edges will become invalid, so they have to be corrected
-- after calling this function.
compileJoin :: MonadGrLang m => RuleDeclaration -> GraphT m (Either [(NodeId, NodeId)] [(EdgeId, EdgeId)])
compileJoin (DeclJoin joined newName) = do
  elems <- getJoinableElems joined
  case elems of
    Left (nodes, ntype) -> do
      let jointName = case newName of
            Nothing -> Text.intercalate "_" (map nodeName nodes)
            Just (A _ name) -> name
      jointId <- createNode ntype (A Nothing jointName)
      modify $ \state ->
        let joinedNames = Map.fromList [ (nodeName n, n) | n <- nodes ]
        in state { grNamedElements = Map.difference (grNamedElements state) joinedNames }
      return $ Left [ (nodeId n, jointId) | n <- nodes ]
    Right (edges, src, tgt, etype) -> do
      jointId <- createEdge etype src tgt Nothing (fmap Ann.drop newName)
      modify $ \state ->
        let joinedNames = Map.fromList [ (edgeName e, e) | e <- edges ]
        in state { grNamedElements = Map.difference (grNamedElements state) joinedNames }
      return $ Right [ (edgeId e, jointId) | e <- edges ]
compileJoin _ = return $ Left []

getJoinableElems :: MonadGrLang m =>
                      [Annotated (Maybe Location) Text]
                      -> GraphT m (Either ([GrNode], NodeType) ([GrEdge], NodeId, NodeId, EdgeType))
getJoinableElems [] = error "compileJoin: join expression with empty list of joined nodes"
getJoinableElems (A loc name : names) = do
  elem <- getElem (A loc name)
  case elem of
    Left (node, ntype) -> getJoinableNodes [node] node ntype names
    Right (edge, etype) -> getJoinableEdges [edge] edge etype names

getJoinableNodes :: MonadGrLang m =>
                    [GrNode]
                    -> GrNode
                    -> NodeType
                    -> [Annotated (Maybe Location) Text]
                    -> GraphT m (Either ([GrNode], NodeType) e)
getJoinableNodes acc _ ntype [] = return $ Left (acc, ntype)
getJoinableNodes acc refNode ntype (A loc name : names) = do
  elem <- getElem (A loc name)
  case elem of
    Left (node, ntype')
      | nodeId ntype' /= nodeId ntype -> throwError loc . PP.fillSep $
          PP.words "Cannot join nodes" ++ [pretty (nodeName refNode), "and", pretty name]
          ++ PP.words "of different types"
      | otherwise -> getJoinableNodes (node:acc) refNode ntype names
    Right _ -> throwError loc . PP.fillSep $
        PP.words "Cannot join node" ++ [pretty (nodeName refNode), "with", "edge", pretty name]

getJoinableEdges :: MonadGrLang m =>
                    [GrEdge]
                    -> Edge (Maybe Metadata)
                    -> EdgeType
                    -> [Annotated (Maybe Location) Text]
                    -> GraphT m (Either n ([GrEdge], NodeId, NodeId, EdgeType))
getJoinableEdges acc refEdge etype [] = return $ Right (acc, sourceId refEdge, targetId refEdge, etype)
getJoinableEdges acc refEdge etype (A loc name : names) = do
  elem <- getElem (A loc name)
  case elem of
    Left _ -> throwError loc . PP.fillSep $
      PP.words "Cannot join edge" ++ [pretty (edgeName refEdge), "with", "node", pretty name]
    Right (edge, etype')
      | edgeId etype' /= edgeId etype -> throwError loc . PP.fillSep $
          PP.words "Cannot join edges" ++ [pretty (edgeName refEdge), "and", pretty name]
          ++ PP.words "of different types"
      | sourceId edge /= sourceId refEdge -> throwError loc . PP.fillSep $
          PP.words "Cannot join edges" ++ [pretty (edgeName refEdge), "and", pretty name]
          ++ PP.words "of different sources"
      | targetId edge /= targetId refEdge -> throwError loc . PP.fillSep $
          PP.words "Cannot join edges" ++ [pretty (edgeName refEdge), "and", pretty name]
          ++ PP.words "of different targets"
      | otherwise -> getJoinableEdges (edge:acc) refEdge etype names

-- | Given the LHS graph with the graph state that was used for creating the LHS graph
-- and a forbid declaration, create a NAC by adding all declared elements to the LHS graph,
-- then including the LHS graph into the extended graph.
compileForbid :: MonadGrLang m => GrGraph -> GraphState -> RuleDeclaration -> ExceptT Error m (Maybe GrMorphism)
compileForbid lhsGraph lhsState (DeclForbid _ decls) = do
  nacGraph <- evalGraphT lhsState (compileGraph' decls)
  return . Just $ TGraph.makeInclusion lhsGraph nacGraph
compileForbid _ _ _ = return Nothing
