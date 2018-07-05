{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Category.LabeledGraph.FindMorphism () where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.List
import           Control.Monad.Reader
import           Data.Maybe                     (mapMaybe)

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Category.LabeledGraph.Category
import           Data.EnumMap                   (EnumMap)
import qualified Data.EnumMap                   as EnumMap
import           Data.EnumSet                   (EnumSet)
import qualified Data.EnumSet                   as EnumSet
import           Data.LabeledGraph              hiding (edgeMap, empty, nodeMap)
import qualified Data.LabeledGraph              as Graph
import           Data.LabeledGraph.Morphism     hiding (edgeMap, nodeMap, variableMap)
import           Data.LabeledGraph.Morphism     as Morphism
import           Data.Variable
import           Util.EnumMap                   as EnumMap


-- | Function that allows building multiple morphisms between two objects.
--
-- * Has 'BuilderConfig' as a global constant
-- * Returns zero or more BuilderStates
type MorphismBuilder = BuilderState -> ReaderT BuilderConfig [] BuilderState

-- | Current state of the morphism finding algorithm.
data BuilderState = State
  { nodeState :: ComponentState NodeId LNode
  , edgeState :: ComponentState EdgeId LEdge
  , varState  :: ComponentState VarId Variable
  }

-- | Restrictions on the morphisms that may be found.
data BuilderConfig =
  Config
    { nodeConfig :: ComponentConfig NodeId LNode
    , edgeConfig :: ComponentConfig EdgeId LEdge
    , varConfig  :: ComponentConfig VarId Variable
    }

-- | Current state of the morphism finding algorithm for one component of the morphism.
data ComponentState id element = CState
  { unmappedDomElements  :: EnumMap id element
  , availableCodElements :: EnumMap id element
  , mapping              :: EnumMap id id
  }

-- | Restrictions on one of the components of the morphisms that may be found.
data ComponentConfig id element = CConfig
  { updateAfterMapping :: id -> id -> ComponentState id element -> ComponentState id element
  , validateFinalState :: EnumSet id -> EnumSet id -> ComponentState id element -> Bool
  }

instance FindMorphism LabeledMorphism where

  findMorphisms restriction domain codomain =
    runMorphismBuilder (makeConfig $ toMorphismType restriction) domain codomain
      mapAllElementsFreely

  findCospanCommuters restriction left right =
    runMorphismBuilder (makeConfig $ toMorphismType restriction) (domain left) (domain right) $
      mapAllFromCospan lookupEdgeId Morphism.edgeMap
        >=> mapAllFromCospan lookupNodeId Morphism.nodeMap
        >=> mapAllFromCospan lookupVarId Morphism.variableMap
    where
      mapAllFromCospan :: Component id elem =>
        (id -> LabeledMorphism -> Maybe id) -> (LabeledMorphism -> EnumMap id id) -> MorphismBuilder
      mapAllFromCospan applyTo getMorphismComponent =
        let
          candidates = EnumMap.inverse . EnumMap.toList $ getMorphismComponent right
          pickCodomainElem state elemLeft =
            case applyTo (idOf elemLeft) left of
              Nothing -> []
              Just elemMid -> case EnumMap.lookup elemMid candidates of
                Nothing -> []
                Just elemsRight -> mapMaybe (`EnumMap.lookup` availableCodElements state) elemsRight
        in mapAll pickCodomainElem

-- | Create a configuration that will produce morphisms of the given type.
makeConfig :: MorphismType -> BuilderConfig
makeConfig restriction = Config config config config
  where
    config :: Component id element => ComponentConfig id element
    config = case restriction of
      AnyMorphism -> CConfig
        { updateAfterMapping = \_ _ -> id
        , validateFinalState = \_ _ _ -> True
        }
      Monomorphism -> CConfig
        { updateAfterMapping = \_ e state ->
            state { availableCodElements = EnumMap.delete e (availableCodElements state) }
        , validateFinalState = \_ _ _ -> True
        }
      Epimorphism -> CConfig
          { updateAfterMapping = \_ _ -> id
          , validateFinalState = \_ codomain state ->
              EnumSet.fromList (EnumMap.elems $ mapping state) == codomain
          }
      Isomorphism -> CConfig
        {  updateAfterMapping = \_ e state ->
            state { availableCodElements = EnumMap.delete e (availableCodElements state) }
        , validateFinalState = \_ _ state -> EnumMap.null (availableCodElements state)
        }
{-# INLINE makeConfig #-}

-- | Execute the given morphism builder to find morphisms between the given graphs, with the given
-- configuration.
runMorphismBuilder :: BuilderConfig -> LabeledGraph -> LabeledGraph -> MorphismBuilder -> [LabeledMorphism]
runMorphismBuilder config domain codomain build =
  let
    initialState = State
      { nodeState = CState
          { unmappedDomElements = EnumMap.fromList (Graph.nodeMap domain)
          , availableCodElements = EnumMap.fromList (Graph.nodeMap codomain)
          , mapping = EnumMap.empty }
      , edgeState = CState
          { unmappedDomElements = EnumMap.fromList (Graph.edgeMap domain)
          , availableCodElements = EnumMap.fromList (Graph.edgeMap codomain)
          , mapping = EnumMap.empty }
      , varState = CState
          { unmappedDomElements = freeVariableMap domain
          , availableCodElements = freeVariableMap codomain
          , mapping = EnumMap.empty }
      }
    in do
      finalState <- runReaderT (build initialState) config
      guard $ validateFinalState (nodeConfig config) (EnumSet.fromList $ nodeIds domain) (EnumSet.fromList $ nodeIds codomain) (nodeState finalState)
      guard $ validateFinalState (edgeConfig config) (EnumSet.fromList $ edgeIds domain) (EnumSet.fromList $ edgeIds codomain) (edgeState finalState)
      guard $ validateFinalState (varConfig config) (freeVariableSet domain) (freeVariableSet codomain) (varState finalState)
      return $
        LabeledMorphism domain codomain (mapping $ nodeState finalState) (mapping $ edgeState finalState) (mapping $ varState finalState)


-- | Maps all unmapped elements of the domain. For each unmapped element of the domain, attempts
-- mapping it to all available elements of the codomain.
mapAllElementsFreely :: MorphismBuilder
mapAllElementsFreely = mapAllEdges >=> mapAllNodes >=> mapAllVariables
  where
    mapAllNodes = mapAll (pickElems @NodeId @LNode)
    mapAllEdges = mapAll (pickElems @EdgeId @LEdge)
    mapAllVariables = mapAll (pickElems @VarId @Variable)
    pickElems :: Enum id => ComponentState id elem -> t -> [elem]
    pickElems state _ = EnumMap.elems (availableCodElements state)
{-# INLINE mapAllElementsFreely #-}


-- | Class describing components of the morphism (e.g. node component, edge component, variable
-- component)
class (Eq id, Enum id) => Component id element | id -> element, element -> id where
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
      | EnumMap.null (unmappedDomElements $ getComponent @id @element state) = return state
      | otherwise = do
          let (_, domainElement) = EnumMap.findMin (unmappedDomElements $ getComponent state)
          codomainElement <- lift $ pickCodomainElement (getComponent state) domainElement
          state' <- addMapping domainElement codomainElement state
          recurse state'
{-# INLINE mapAll #-}


-- | Try to add a mapping between the elements with given identifiers. If the domain element is
-- already mapped, ensures that the mapping is correct. Never creates mappings to unavailable
-- codomain elements.
addInducedMapping :: Component id element => id -> id -> MorphismBuilder
addInducedMapping domainId codomainId state =
  case EnumMap.lookup domainId (unmappedDomElements $ getComponent state) of
    Nothing -> do
      guard $ EnumMap.lookup domainId (mapping $ getComponent state) == Just codomainId
      return state
    Just domainNode ->
      case EnumMap.lookup codomainId (availableCodElements $ getComponent state) of
        Just codomainNode -> addMapping domainNode codomainNode state
        Nothing           -> empty

-- | Insert a mapping between elements with the given identifiers. Assumes the domain element is
-- unmapped and the codomain element is available.
insertMapping :: Component id element => id -> id -> MorphismBuilder
insertMapping domainId codomainId state = do
  let componentState = getComponent state
  componentConfig <- asks getConfig
  let updatedState = updateAfterMapping componentConfig domainId codomainId $ componentState
        { mapping = EnumMap.insert domainId codomainId (mapping componentState)
        , unmappedDomElements = EnumMap.delete domainId (unmappedDomElements componentState)
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
      (Nothing, _)      -> return state0 -- Unlabeled nodes can be mapped to labeled nodes
      (Just _, Nothing) -> empty -- Labeled nodes cannot be mapped to unlabeled nodes
      (Just v, Just w)  -> addInducedMapping (varId v) (varId w) state0
    insertMapping (nodeId domainNode) (nodeId codomainNode) state1

instance Component VarId Variable where
  getComponent = varState
  setComponent newVarState state = state { varState = newVarState }
  getConfig = varConfig
  idOf = varId
  addMapping v w = insertMapping (varId v) (varId w)
