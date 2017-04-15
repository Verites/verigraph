module SymbolicGraph.Morphism
  (
  -- * Morphism type
    SymbolicMorphism

  -- * Query
  , isTotal
  , applyToNode
  , applyToEdge
  , applyToVariable
  , applyToNodeId
  , applyToEdgeId
  , lookupNodeId
  , lookupEdgeId
  , isNodeDefined
  , isEdgeDefined

  -- * Construction
  , SymbolicGraph.Morphism.Internal.id
  , fromGraphsAndLists

  -- * Conversion
  -- ** Maps
  , nodeMap
  , edgeMap
  , variableMap

  -- ** Relations
  , nodeRelation
  , edgeRelation
  , variableRelation

  -- ** Lists
  , orphanNodes
  , orphanEdges
  , orphanVariables
  ) where

import           SymbolicGraph.Morphism.Cocomplete ()
import           SymbolicGraph.Morphism.Internal
