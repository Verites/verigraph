{- | This module defines the category \(\mathbf{Graph}_T\) of typed graphs.

This is intended to be imported qualified, to avoid name clashes, e.g.

> import Category.TypedGraph 
> import Category.TypedGraph as TGraph

-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Category.TypedGraph
( TypedGraph
, Morphism
, CatM
, Config(..)
, MatchRestriction(..)
, runCat
, getTypeGraph
)  where

import Control.Monad
import Control.Monad.Trans

import           Abstract.Category.NewClasses
import           Base.Valid
import           Base.Isomorphic
import qualified Data.Graphs                        as Graph
import qualified Data.Graphs.Morphism               as Graph
import           Data.TypedGraph                    hiding (null)
import           Data.TypedGraph.Morphism
import           Category.TypedGraph.Category
import           Category.TypedGraph.FindMorphism ()
import           Category.TypedGraph.FinalPullbackComplement ()
import           Category.TypedGraph.Limit ()
import           Category.TypedGraph.Adhesive ()

instance {-# OVERLAPS #-} Valid (CatM n e) (TypedGraph n e) where
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

instance {-# OVERLAPS #-} IsoM (CatM n e) (TypedGraph n e) where
  isIso g1 g2 = not . null <$> findMorphisms iso g1 g2
