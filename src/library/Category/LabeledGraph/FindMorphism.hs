{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Category.LabeledGraph.FindMorphism () where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.List
import           Control.Monad.Reader
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (mapMaybe)
import           Data.Set                               (Set)
import qualified Data.Set                               as Set

import           Abstract.Category.FinitaryCategory
import           Category.LabeledGraph.FinitaryCategory ()
import qualified Data.EnumMap                           as EnumMap
import           Data.LabeledGraph                      hiding (edgeMap, empty, nodeMap)
import qualified Data.LabeledGraph                      as Graph
import           Data.LabeledGraph.Morphism             hiding (edgeMap, nodeMap, variableMap)
import           Data.LabeledGraph.Morphism             as Morphism
import           Data.Variable
import           Util.Map                               as Map


-- | Function that allows building multiple morphisms between two objects.
--
-- * Has 'BuilderConfig' as a global constant
-- * Returns zero or more BuilderStates
type MorphismBuilder = BuilderState -> ReaderT BuilderConfig [] BuilderState

-- | Current state of the morphism finding algorithm.
data BuilderState = State
  { nodeState :: ComponentState NodeId LNode
  , edgeState :: ComponentState EdgeId LEdge
  , varState  :: ComponentState Variable Variable
  }

-- | Restrictions on the morphisms that may be found.
data BuilderConfig =
  Config
    { nodeConfig :: ComponentConfig NodeId LNode
    , edgeConfig :: ComponentConfig EdgeId LEdge
    , varConfig  :: ComponentConfig Variable Variable
    }

-- | Current state of the morphism finding algorithm for one component of the morphism.
data ComponentState id element = CState
  { unmappedDomElements  :: Map id element
  , availableCodElements :: Map id element
  , mapping              :: Map id id
  }

-- | Restrictions on one of the components of the morphisms that may be found.
data ComponentConfig id element = CConfig
  { updateAfterMapping :: id -> id -> ComponentState id element -> ComponentState id element
  , validateFinalState :: Set id -> Set id -> ComponentState id element -> Bool
  }

instance FindMorphism LabeledMorphism where

  findMorphisms restriction domain codomain =
    runMorphismBuilder (makeConfig restriction) domain codomain
      mapAllElementsFreely

  findCospanCommuter restriction left right =
    runMorphismBuilder (makeConfig restriction) (domain left) (domain right) $
      mapAllFromCospan lookupEdgeId (EnumMap.toList . Morphism.edgeMap)
        >=> mapAllFromCospan lookupNodeId (EnumMap.toList . Morphism.nodeMap)
        >=> mapAllFromCospan applyToVariable (Map.toList . Morphism.variableMap)
    where
      mapAllFromCospan :: Component id elem =>
        (id -> LabeledMorphism -> Maybe id) -> (LabeledMorphism -> [(id, id)]) -> MorphismBuilder
      mapAllFromCospan applyTo getMorphismComponent =
        let
          candidates = Map.inverse (getMorphismComponent right)
          pickCodomainElem state elemLeft =
            case applyTo (idOf elemLeft) left of
              Nothing -> []
              Just elemMid -> case Map.lookup elemMid candidates of
                Nothing -> []
                Just elemsRight -> mapMaybe (`Map.lookup` availableCodElements state) elemsRight
        in mapAll pickCodomainElem


  partialInjectiveMatches nac match =
    runMorphismBuilder (makeConfig Monomorphism) (codomain nac) (codomain match) $
      addMappingsFromSpan >=> mapAllElementsFreely
    where
      addMappingsFromSpan :: MorphismBuilder
      addMappingsFromSpan state = do
        state1 <- foldM induceMapping state (inducedPairs applyToEdgeId Graph.edgeIds)
        state2 <- foldM induceMapping state1 (inducedPairs applyToNodeId Graph.nodeIds)
        foldM induceMapping state2 (inducedPairs applyToVariable freeVariablesOf)
      induceMapping state (domainElem, codomainElem) =
        case Map.lookup (idOf domainElem) (mapping $ getComponent state) of
          Nothing -> addMapping domainElem codomainElem state
          Just previousMapping -> do
            guard (previousMapping == idOf codomainElem)
            return state
      inducedPairs applyTo getElems =
        [ (x, y)
            | z <- getElems (domain nac)
            , Just x <- [applyTo z nac]
            , Just y <- [applyTo z match] ]


-- | Create a configuration that will produce morphisms of the given type.
makeConfig :: MorphismType -> BuilderConfig
makeConfig restriction = Config config config config
  where
    config :: Component id element => ComponentConfig id element
    config = case restriction of
      GenericMorphism -> CConfig
        { updateAfterMapping = \_ _ -> id
        , validateFinalState = \_ _ _ -> True
        }
      Monomorphism -> CConfig
        { updateAfterMapping = \_ e state ->
            state { availableCodElements = Map.delete e (availableCodElements state) }
        , validateFinalState = \_ _ _ -> True
        }
      Epimorphism -> CConfig
          { updateAfterMapping = \_ _ -> id
          , validateFinalState = \_ codomain state ->
              Set.fromList (Map.elems $ mapping state) == codomain
          }
      Isomorphism -> CConfig
        {  updateAfterMapping = \_ e state ->
            state { availableCodElements = Map.delete e (availableCodElements state) }
        , validateFinalState = \_ _ state -> Map.null (availableCodElements state)
        }
{-# INLINE makeConfig #-}

-- | Execute the given morphism builder to find morphisms between the given graphs, with the given
-- configuration.
runMorphismBuilder :: BuilderConfig -> LabeledGraph -> LabeledGraph -> MorphismBuilder -> [LabeledMorphism]
runMorphismBuilder config domain codomain build =
  let
    initialState = State
      { nodeState = CState
          { unmappedDomElements = Map.fromList (Graph.nodeMap domain)
          , availableCodElements = Map.fromList (Graph.nodeMap codomain)
          , mapping = Map.empty }
      , edgeState = CState
          { unmappedDomElements = Map.fromList (Graph.edgeMap domain)
          , availableCodElements = Map.fromList (Graph.edgeMap codomain)
          , mapping = Map.empty }
      , varState = CState
          { unmappedDomElements = Map.fromList [ (v, v) | v <- freeVariablesOf domain ]
          , availableCodElements = Map.fromList [ (v, v) | v <- freeVariablesOf codomain ]
          , mapping = Map.empty }
      }
    in do
      finalState <- runReaderT (build initialState) config
      guard $ validateFinalState (nodeConfig config) (Set.fromList $ nodeIds domain) (Set.fromList $ nodeIds codomain) (nodeState finalState)
      guard $ validateFinalState (edgeConfig config) (Set.fromList $ edgeIds domain) (Set.fromList $ edgeIds codomain) (edgeState finalState)
      guard $ validateFinalState (varConfig config) (freeVariableSet domain) (freeVariableSet codomain) (varState finalState)
      return $
        LabeledMorphism domain codomain
          (EnumMap.fromList . Map.toList . mapping $ nodeState finalState)
          (EnumMap.fromList . Map.toList . mapping $ edgeState finalState)
          (mapping $ varState finalState)


-- | Maps all unmapped elements of the domain. For each unmapped element of the domain, attempts
-- mapping it to all available elements of the codomain.
mapAllElementsFreely :: MorphismBuilder
mapAllElementsFreely = mapAllEdges >=> mapAllNodes >=> mapAllVariables
  where
    mapAllEdges = mapAll (pickElems :: ComponentState EdgeId LEdge -> LEdge -> [LEdge])
    mapAllNodes = mapAll (pickElems :: ComponentState NodeId LNode -> LNode -> [LNode])
    mapAllVariables = mapAll (pickElems :: ComponentState Variable Variable -> Variable -> [Variable])
    pickElems state _ = Map.elems (availableCodElements state)
{-# INLINE mapAllElementsFreely #-}


-- | Class describing components of the morphism (e.g. node component, edge component, variable
-- component)
class (Ord id) => Component id element | id -> element, element -> id where
  getComponent :: BuilderState -> ComponentState id element
  setComponent :: ComponentState id element -> BuilderState -> BuilderState
  getConfig :: BuilderConfig -> ComponentConfig id element
  idOf :: element -> id

  -- | Adds a mapping between the given elements. Assumes the domain element is unmapped and the
  -- codomain element is available. Induces necessary mappings on other components.
  addMapping :: element -> element -> MorphismBuilder


-- | Maps all unmapped elements of the domain. Uses the given function to determine which elements
-- of the codomain are appropriate for each element of the domain.
mapAll :: forall id element. Component id element
  => (ComponentState id element -> element -> [element]) -> MorphismBuilder
mapAll pickCodomainElement =
  recurse
  where
    recurse :: MorphismBuilder
    recurse state
      | Map.null (unmappedDomElements $ getComponent state :: Map id element) = return state
      | otherwise = do
          let (_, domainElement) = Map.findMin (unmappedDomElements $ getComponent state)
          codomainElement <- lift $ pickCodomainElement (getComponent state) domainElement
          state' <- addMapping domainElement codomainElement state
          recurse state'
{-# INLINE mapAll #-}


-- | Try to add a mapping between the elements with given identifiers. If the domain element is
-- already mapped, ensures that the mapping is correct. Never creates mappings to unavailable
-- codomain elements.
addInducedMapping :: Component id element => id -> id -> MorphismBuilder
addInducedMapping domainId codomainId state =
  case Map.lookup domainId (unmappedDomElements $ getComponent state) of
    Nothing -> do
      guard $ Map.lookup domainId (mapping $ getComponent state) == Just codomainId
      return state
    Just domainNode ->
      case Map.lookup codomainId (availableCodElements $ getComponent state) of
        Just codomainNode -> addMapping domainNode codomainNode state
        Nothing -> empty

-- | Insert a mapping between elements with the given identifiers. Assumes the domain element is
-- unmapped and the codomain element is available.
insertMapping :: Component id element => id -> id -> MorphismBuilder
insertMapping domainId codomainId state = do
  let componentState = getComponent state
  componentConfig <- asks getConfig
  let updatedState = updateAfterMapping componentConfig domainId codomainId $ componentState
        { mapping = Map.insert domainId codomainId (mapping componentState)
        , unmappedDomElements = Map.delete domainId (unmappedDomElements componentState)
        }
  return (setComponent updatedState state)


instance Component EdgeId LEdge where
  getComponent = edgeState
  setComponent newEdgeState state = state { edgeState = newEdgeState }
  getConfig = edgeConfig
  idOf = edgeId
  addMapping domainEdge codomainEdge state0 = do
    state1 <- addInducedMapping (sourceId domainEdge) (sourceId codomainEdge) state0
    state2 <- addInducedMapping (targetId domainEdge) (targetId codomainEdge) state1
    insertMapping (edgeId domainEdge) (edgeId codomainEdge) state2

instance Component NodeId LNode where
  getComponent = nodeState
  setComponent newNodeState state = state { nodeState = newNodeState }
  getConfig = nodeConfig
  idOf = nodeId
  addMapping domainNode codomainNode state0 = do
    state1 <- case (nodeLabel domainNode, nodeLabel codomainNode) of
      (Nothing, _) -> return state0 -- Unlabeled nodes can be mapped to labeled nodes
      (Just _, Nothing) -> empty -- Labeled nodes cannot be mapped to unlabeled nodes
      (Just v, Just w) -> addInducedMapping v w state0
    insertMapping (nodeId domainNode) (nodeId codomainNode) state1

instance Component Variable Variable where
  getComponent = varState
  setComponent newVarState state = state { varState = newVarState }
  getConfig = varConfig
  idOf = id
  addMapping = insertMapping
