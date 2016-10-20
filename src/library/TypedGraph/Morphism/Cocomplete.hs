module TypedGraph.Morphism.Cocomplete (

  createEquivalenceNodes, constructNodes, findEquivalenceClass, mergeEquivalences

)

where

import Abstract.Cocomplete
import Data.Set
import           Graph.Graph         as G
import           TypedGraph.Graph
import           TypedGraph.Morphism.Core

instance Cocomplete (TypedGraphMorphism a b) where

  calculateCoequalizer = calculateCoEq'

calculateCoEq' :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
calculateCoEq' f g = g
  where
    objectX = getCodomain g
    nodesOfX = nodesWithType objectX
    edgesOfX = edgesWithType objectX

createEquivalenceNodes :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Set[(NodeId,NodeId)]
createEquivalenceNodes f g = initialX
  where
    initialX = fromList $ Prelude.map (:[]) (nodesWithType (getCodomain f)) -- (nodeId, typeNodeId) from B
    nodesFromA = fromList $ nodesWithType (getDomain f)            -- (nodeId,typeNodeId) from A
    equivalentNodes (n,nt) = ((applyNodeUnsafe f n,nt), (applyNodeUnsafe g n,nt)) -- ((nodeAndTypeFromA),(nodeandTypeFromB))
    toGluingOnB = Data.Set.map equivalentNodes nodesFromA







type EquivalenceClass a = Set(a,a) -- Element, elementType

-- Receives a set @b@ of pairs of nodes to be glued in X, a set @i@ of the initial nodes in X and return
-- the new set of nodes in X glued according to @b@
constructNodes :: Set((NodeId,NodeId),(NodeId,NodeId)) -> Set (EquivalenceClass NodeId) -> Set (EquivalenceClass NodeId)
constructNodes toBeGlued toBeX
  | Data.Set.null toBeGlued = toBeX
  | otherwise = constructNodes (restSet toBeGlued) (merge (fstSet toBeGlued) toBeX)
  where
    fstSet = elemAt 0
    restSet set = set `difference` singleton (fstSet set)
    merge (e1,e2) s =  s `diff` e1 `diff` e2 `union` mergeEquivalences (e1,e2) s
    diff s e = difference s (singleton $ findEquivalenceClass e s)

mergeEquivalences :: ((NodeId,NodeId), (NodeId,NodeId)) -> Set(EquivalenceClass NodeId) -> Set(EquivalenceClass NodeId)
mergeEquivalences (e1,e2) set = singleton(findEquivalenceClass e1 set `union` findEquivalenceClass e2 set)

--  otherwise construct ps $ (findEquivalenceClass (fst p) list ++ findEquivalenceClass (snd p) list) : newList
--  where
--    newList = (list \\ [findEquivalenceClass (fst p) list]) \\ [findEquivalenceClass (snd p) list]
findEquivalenceClass :: (NodeId,NodeId) -> Set(EquivalenceClass NodeId) -> EquivalenceClass NodeId
findEquivalenceClass element set
  | Data.Set.null set = error "the set shouldn't be empty"
  | otherwise = elemAt 0 $ Data.Set.filter (element `elem`) set
