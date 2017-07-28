{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraph
( TypedGraph
, TypedGraphMorphism
, TGraphCat
, TGraphConfig(..)
, runCat
, getTypeGraph
)  where

import Control.Monad
import Control.Monad.Trans

import           Abstract.Category.NewClasses
import           Base.Valid
import qualified Data.Graphs                        as Graph
import qualified Data.Graphs.Morphism               as Graph
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Category.TypedGraph.Category
import           Category.TypedGraph.FindMorphism ()
import           Category.TypedGraph.Limit ()
import           Category.TypedGraph.Adhesive ()


instance {-# OVERLAPS #-} Valid (TGraphCat n e) (TypedGraph n e) where
  validator graph = do
    catTypeGraph <- lift getTypeGraph
    ensure (typeGraph graph == catTypeGraph) "wrong type graph for the category"

    forM_ (untypedNodes graph) $ \nodeId ->
      withContext ("node #" ++ show nodeId) $
        case Graph.applyNodeId graph nodeId of
          Nothing -> invalid "has undefined type"
          Just typeId -> ensure (Graph.isNodeOf catTypeGraph typeId) ("has invalid type #" ++ show typeId)
    
    let ugraph = untypedGraph graph
    forM_ (Graph.edges ugraph) $ \(Graph.Edge edgeId srcId tgtId _) ->
      withContext ("edge #" ++ show edgeId) $
        case Graph.lookupEdge edgeId ugraph >>= Graph.applyEdge graph of
          Nothing -> invalid "has undefined type"
          Just (Graph.Edge typeId srcTypeId tgtTypeId _) -> do
            ensure (Graph.isEdgeOf catTypeGraph typeId) ("has invalid type #" ++ show typeId)
            ensure (Graph.applyNodeId graph srcId == Just srcTypeId) "has source of invalid type"
            ensure (Graph.applyNodeId graph tgtId == Just tgtTypeId) "has target of invalid type"
