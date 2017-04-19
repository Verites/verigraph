module LabeledGraph.Morphism
  (
  -- * Morphism type
    LabeledMorphism

  -- * Query
  , isTotal
  , isTotalOnNodes
  , isTotalOnEdges
  , isTotalOnVariables
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
  , LabeledGraph.Morphism.Internal.id
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

import           LabeledGraph.Morphism.FindMorphism ()
import           LabeledGraph.Morphism.Internal
