module Graph.Rewriting where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map  as M 
import qualified Abstract.Relation as R
import qualified Abstract.Morphism as Mor
import qualified Graph.Graph as G  
import qualified Graph.GraphMorphism as GM  
import qualified Graph.TypedGraphMorphism as TGM 
import qualified Graph.GraphRule as GR 


-------- Delete elements from relations ----------


-- | Remove an element from the domain of the relation 
removeDom :: (Eq a, Ord a) => a -> R.Relation a -> R.Relation a
removeDom x r = 
 let d = R.domain r
     c = R.codomain r
     m = R.mapping r
 in  R.Relation (L.delete x d) c (M.delete x m)


-- | Remove an element from the codomain of the relation
removeCod :: (Eq a,Ord a) => a -> R.Relation a -> R.Relation a
removeCod x r = 
 let d = R.domain r
     c = R.codomain r
     m = R.mapping r
  in R.Relation d (L.delete x c) (M.map (L.delete x) m)


----- Delete elements from graph morphisms -------


-- | Remove an edge from the domain of the morphism
removeEdgeDom :: G.EdgeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
removeEdgeDom e gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in  GM.GraphMorphism (G.removeEdge e g1) g2 nm (removeDom e em)


-- | Remove an edge from the codomain of the morphism
removeEdgeCod :: G.EdgeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
removeEdgeCod e gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in  GM.GraphMorphism g1 (G.removeEdge e g2) nm (removeCod e em)


-- | Remove a node from the domain of the morphism
removeNodeDom :: G.NodeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
removeNodeDom n gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in GM.GraphMorphism (G.removeNode n g1) g2 (removeDom n nm) em 


-- | Remove a node from the codomain of the morphism
removeNodeCod :: G.NodeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
removeNodeCod n gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in GM.GraphMorphism g1 (G.removeNode n g2) (removeCod n nm) em








------ Delete elements from typed graph morphisms -------


-- | Remove a node from the domain of a typed graph morphism
removeNodeDomTyped :: G.NodeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
removeNodeDomTyped n tgm  =
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in TGM.typedMorphism (removeNodeDom n dom) cod (removeNodeDom n m)  


-- | Remove an edge from the domain of a typed graph morphism
removeEdgeDomTyped :: G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
removeEdgeDomTyped e tgm =
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in  TGM.typedMorphism (removeEdgeDom e dom) cod (removeEdgeDom e m)  


-- | Remove a node from the codomain of a typed graph morphism
removeNodeCodTyped :: G.NodeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
removeNodeCodTyped n tgm = 
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in  TGM.typedMorphism dom (removeNodeDom n cod) (removeNodeCod n m)  


-- | Remove an edge from the domain of a typed graph morphism
removeEdgeCodTyped :: G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
removeEdgeCodTyped e tgm =
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in  TGM.typedMorphism dom (removeEdgeDom e cod) (removeEdgeCod e m)  


-------- Query rules for deleted and created elements ----------


-- | Return the elements in the domain which are not in the image of the relation (orphans)
orphans :: (Eq a) => R.Relation a -> [a]
orphans r = (L.\\) (R.codomain r)  (R.image r)


-- | Return the orphan nodes in a graph morphism
orphanNodes :: GM.GraphMorphism a b -> [G.NodeId]
orphanNodes gm = orphans (GM.nodeRelation gm)


-- | Return the orphan edges in a graph morphism
orphanEdges :: GM.GraphMorphism a b -> [G.EdgeId]
orphanEdges gm = orphans (GM.edgeRelation gm)


-- | Return the orphan nodes in a typed graph morphism
orphanNodesTyped :: TGM.TypedGraphMorphism a b -> [G.NodeId]
orphanNodesTyped tgm = orphanNodes (TGM.mapping tgm)


-- | Return the orphan edges in a typed graph morphism
orphanEdgesTyped :: TGM.TypedGraphMorphism a b -> [G.EdgeId]
orphanEdgesTyped tgm = orphanEdges (TGM.mapping tgm)


-- | Return the nodes deleted by a rule
deletedNodes :: GR.GraphRule a b -> [G.NodeId]
deletedNodes r = orphanNodesTyped (GR.left r)


-- | Return the nodes created by a rule
createdNodes :: GR.GraphRule a b -> [G.NodeId]
createdNodes r = orphanNodesTyped (GR.right r)


-- | Return the edges deleted by a rule
deletedEdges :: GR.GraphRule a b -> [G.EdgeId]
deletedEdges r = orphanEdgesTyped (GR.left r)


-- | Return the edges created by a rule
createdEdges :: GR.GraphRule a b -> [G.EdgeId]
createdEdges r = orphanEdgesTyped (GR.right r)


---- Convenient instances of Number and Enum classes for NodeId and EdgeId

instance Num (G.NodeId) where
  (G.NodeId x)  +  (G.NodeId y) = G.NodeId (x+y)
  (G.NodeId x)  *  (G.NodeId y) = G.NodeId (x*y)
  (G.NodeId x)  -  (G.NodeId y) = G.NodeId (x-y)
  negate (G.NodeId x) = G.NodeId (negate x)
  signum (G.NodeId x) = G.NodeId (signum x)
  fromInteger x       = (G.NodeId $ fromIntegral x)
  abs (G.NodeId x)    = G.NodeId (abs x)

instance Enum (G.NodeId) where
  toEnum x = G.NodeId x
  fromEnum (G.NodeId x) = x 

instance Num (G.EdgeId) where
  (G.EdgeId x)  +  (G.EdgeId y) = G.EdgeId (x+y)
  (G.EdgeId x)  *  (G.EdgeId y) = G.EdgeId (x*y)
  (G.EdgeId x)  -  (G.EdgeId y) = G.EdgeId (x-y)
  negate (G.EdgeId x) = G.EdgeId (negate x)
  signum (G.EdgeId x) = G.EdgeId (signum x)
  fromInteger x       = G.EdgeId $ fromIntegral x
  abs (G.EdgeId x)    = G.EdgeId (abs x)

instance Enum (G.EdgeId) where
  toEnum x = G.EdgeId x
  fromEnum (G.EdgeId x) = x 


------ Infinite lists containing new nodes and edges for a graph --------


-- | Infinite list of new node instances of a graph
newNodes :: G.Graph a b -> [G.NodeId]
newNodes g = [succ maxNode..]
  where maxNode = foldr max 0 (G.nodes g)

 
-- | Infinite list of new node instances of a typed graph
newNodesTyped :: GM.TypedGraph a b -> [G.NodeId]
newNodesTyped tg = newNodes $ Mor.domain tg


-- | Infinite list of new edge instances of a graph
newEdges :: G.Graph a b -> [G.EdgeId]
newEdges g = [succ maxEdge..]
  where maxEdge = foldr max 0 (G.edges g)

 
-- | Infinite list of new edge instances of a typed graph
newEdgesTyped :: GM.TypedGraph a b -> [G.EdgeId]
newEdgesTyped tg = newEdges $ Mor.domain tg


------ Invertion of graph morphisms

invertGM :: GM.GraphMorphism a b -> GM.GraphMorphism a b
invertGM gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in GM.GraphMorphism g2 g1 (R.inverse nm) (R.inverse em)


invertTGM :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
invertTGM tgm = 
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in TGM.typedMorphism cod dom (invertGM m)


------------ Pushout Complement -------------

{- 
   algorithm:
   1. compose l and m generating ml
   2. query edges for deletion in the codomain of ml
   2. query nodes for deletion in the codomain of ml
   3. delete all edges
   4. delete all nodes
-}


-- | Pushout complement:  G <-m- L,  L <-l- K,  ====>   D <-k- K   
-- this code assumes that l is injective

poc :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b ->  TGM.TypedGraphMorphism a b
poc m l = 
  let ml       = Mor.compose l m                                                     -- compose l and m obtaining ml
      delEdges = catMaybes $ map (GM.applyEdge $ TGM.mapping m) (orphanEdgesTyped l) -- obtain list of edges to be deleted in G
      delNodes = catMaybes $ map (GM.applyNode $ TGM.mapping m) (orphanNodesTyped l) -- obtain list of nodes to be deleted in G
  in foldr (removeNodeCodTyped)                                                      -- delete all edges, then all nodes from ml
       (foldr (removeEdgeCodTyped) ml delEdges) 
         delNodes
      
  

------------ Pushout  ------------------------

{- 
   algorithm:
   1. invert r
   2. compose k and r^-1
   3. create node table  (R -> G')
   5. create edge table  (R -> G')
   4. associate nodes
   6. associate edges
-}

-- | Pushout : D <-k- K,   K -r-> R ,  ====>  G' <-m'- R  (comatch)

po :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b ->  TGM.TypedGraphMorphism a b
po k r = 
  let 
      kr = Mor.compose (invertTGM r) k                                   -- invert r and compose with k, obtain kr : R -> D
      createdNodes = orphanNodesTyped r                                  -- nodes in R to be created
      createdEdges = orphanEdgesTyped r                                  -- edges in R to be created
      nodeTable    = zip createdNodes (newNodesTyped $ Mor.codomain kr)  -- table mapping NodeIds in R to NodeIds in G'
      edgeTable    = zip createdEdges (newEdgesTyped $ Mor.codomain kr)  -- table mapping EdgeIds in R to EdgeIds in G'   

      -- generate new node instances in G', associating them to the "created" nodes in R
      kr'          = foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNode (Mor.domain kr) a 
                                          in updateNodeRelationTGM a b tp tgm)
                                          
                           kr 
                           nodeTable
      
      -- query the instance graphs R
      typemor = Mor.domain         kr'                     -- typemor is the typed graph (R -> T)
      g       = Mor.domain         typemor                 -- g  is the instance graph R 
      mp      = TGM.mapping        kr'                     -- mp is the mapping of kr'  : (R -> D'), where D' = D + new nodes
      s1 e = fromJust $ G.sourceOf g e                     -- obtain source of e in R
      t1 e = fromJust $ G.targetOf g e                     -- obtain target of e in R
      s2 e = fromJust $ GM.applyNode mp (s1 e)             -- obtain source of m'(e) in G'
      t2 e = fromJust $ GM.applyNode mp (s2 e)             -- obtain target of m'(e) in G'
      tp e = fromJust $ GM.applyEdge typemor e             -- obtain type of e in R

      -- generate new edge table with new information
      edgeTable' = map (\(e,e2) -> (e, s1 e, t1 e, e2, s2 e,t2 e, tp e)) edgeTable

      -- create new morphism adding all edges
      kr''      = foldr (\(a,sa,ta,b,sb,tb,tp) tgm ->  updateEdgeRelationTGM a b (createEdgeCodTGM b sb tb tp tgm) )
                        kr'
                        edgeTable'
                                
  in kr''





-- | Pushout

dpo :: TGM.TypedGraphMorphism a b -> GR.GraphRule a b -> TGM.TypedGraphMorphism a b
dpo m rule = 
  let
     l = GR.left rule
     r = GR.right rule
  in po (poc m l) r




---- Insertion of nodes in graph morphisms

updateNodeRelationGM :: G.NodeId -> G.NodeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
updateNodeRelationGM n1 n2 gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in GM.GraphMorphism (G.insertNode n1 g1) (G.insertNode n2 g2) (R.update n1 n2 nm) em

updateNodeRelationTGM :: G.NodeId -> G.NodeId -> G.NodeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b 
updateNodeRelationTGM n1 n2 tp tgm = 
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in TGM.typedMorphism (updateNodeRelationGM n1 tp dom) 
                       (updateNodeRelationGM n2 tp cod) 
                       (updateNodeRelationGM n1 n2 m)


---- Insertion of edges in graph morphisms


-- | This function adds an edge e1 (with source s1 and target t1) to the domain of the morphism, and associate it to e2
--   It assumes s1, t1, e2 already exist, and that e1 does not exist.
createEdgeDom :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
createEdgeDom e1 s1 t1 e2 gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in  GM.GraphMorphism (G.insertEdge e1 s1 t1 g1) g2 nm (R.update e1 e2 em)


-- | This function adds an edge e2 (with source s2 and target t2) to the codomain of the morphism. 
--   It assumes that s2,t2 exist, and that e2 does not exist
createEdgeCod :: G.EdgeId -> G.NodeId -> G.NodeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
createEdgeCod e2 s2 t2 gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
      dom = R.domain em
      cod = R.codomain em
      m   = R.mapping em
  in  GM.GraphMorphism g1 (G.insertEdge e2 s2 t2 g2) nm (R.Relation dom (L.union [e2] cod) m)


-- | modifies a graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelationGM :: G.EdgeId -> G.EdgeId -> GM.GraphMorphism a b -> GM.GraphMorphism a b
updateEdgeRelationGM e1 e2 gm = 
  let g1 = Mor.domain gm
      g2 = Mor.codomain gm
      nm = GM.nodeRelation gm
      em = GM.edgeRelation gm
  in GM.GraphMorphism g1 g2 nm (R.update e1 e2 em)



---- Insertion of edges in typed graph morphisms


-- | This function adds an edge e1 (with source s1, target t1 and type tp) to the domain of the typed graph morphism, and associate it to e2
--   It assumes s1, t1, e2, tp already exist, and that e1 does not exist.
createEdgeDomTGM :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
createEdgeDomTGM e1 s1 t1 tp e2 tgm = 
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in  TGM.TypedGraphMorphism (createEdgeDom e1 s1 t1 tp dom)
                             cod
                             (createEdgeDom e1 s1 t1 e2 m)
                      

-- | This function adds an edge e2 (with source s2, target t2 and type tp) to the codomain of the typed graph morphism
--   It assumes s2, t2, tp already exist, and that e2 does not exist.
createEdgeCodTGM :: G.EdgeId -> G.NodeId -> G.NodeId -> G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
createEdgeCodTGM e2 s2 t2 tp tgm = 
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in  TGM.TypedGraphMorphism dom 
                             (createEdgeCod e2 s2 t2 cod)
                             (createEdgeDom e2 s2 t2 tp m)

-- | updates a typed graph morphism, mapping edge e1 to edge e2. It assumes both edges already exist.
updateEdgeRelationTGM :: G.EdgeId -> G.EdgeId -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
updateEdgeRelationTGM e1 e2 tgm = 
  let dom = Mor.domain tgm
      cod = Mor.codomain tgm 
      m   = TGM.mapping tgm
  in  TGM.TypedGraphMorphism dom cod (updateEdgeRelationGM e1 e2 m)


