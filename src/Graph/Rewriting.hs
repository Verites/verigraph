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

-- | Maps elements on themselves
idMap :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
idMap m k = TGM.typedMorphism (Mor.codomain k) (Mor.codomain m) edgesUpdate
    where
        graph = Mor.domain (Mor.codomain m)
        init = GM.empty graph graph
        nodes = G.nodes graph
        edges = G.edges graph
        nodesUpdate = foldl (\gm n -> GM.updateNodes n n gm) init nodes
        edgesUpdate = foldl (\gm e -> GM.updateEdges e e gm) nodesUpdate edges

------------ Pushout Complement -------------
{- 
   algorithm:
   1. compose l and m generating ml
   2. query edges for deletion in the codomain of ml
   2. query nodes for deletion in the codomain of ml
   3. delete all edges
   4. delete all nodes
-}

-- | Pushout complement:  G <-m- L,  L <-l- K,  ====>   (D <-k- K, G <-l'- D)
-- this code assumes that l is injective

poc :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
poc m l = 
  let ml       = Mor.compose l m                                                         -- compose l and m obtaining ml
      delEdges = catMaybes $ map (GM.applyEdge $ TGM.mapping m) (TGM.orphanEdgesTyped l) -- obtain list of edges to be deleted in G
      delNodes = catMaybes $ map (GM.applyNode $ TGM.mapping m) (TGM.orphanNodesTyped l) -- obtain list of nodes to be deleted in G
      k        = foldr (TGM.removeNodeCodTyped)                                          -- delete all edges, then all nodes from ml
                     (foldr (TGM.removeEdgeCodTyped) ml delEdges) 
                         delNodes
  in (k, idMap m k)



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
po :: TGM.TypedGraphMorphism a b -> TGM.TypedGraphMorphism a b
   -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
po k r = 
  let 
      kr = Mor.compose (TGM.invertTGM r) k                                 -- invert r and compose with k, obtain kr : R -> D
      createdNodes = TGM.orphanNodesTyped r                                -- nodes in R to be created
      createdEdges = TGM.orphanEdgesTyped r                                -- edges in R to be created
      nodeTable    = zip createdNodes (GM.newNodesTyped $ Mor.codomain kr) -- table mapping NodeIds in R to NodeIds in G'
      edgeTable    = zip createdEdges (GM.newEdgesTyped $ Mor.codomain kr) -- table mapping EdgeIds in R to EdgeIds in G'   

      -- generate new node instances in G', associating them to the "created" nodes in R
      kr'          = foldr (\(a,b) tgm -> let tp = fromJust $ GM.applyNode (Mor.domain kr) a 
                                          in TGM.updateNodeRelationTGM a b tp tgm)
                           kr 
                           nodeTable
      
      -- query the instance graphs R
      typemor = Mor.domain         kr'                     -- typemor is the typed graph (R -> T)
      g       = Mor.domain         typemor                 -- g  is the instance graph R 
      mp      = TGM.mapping        kr'                     -- mp is the mapping of kr'  : (R -> D'), where D' = D + new nodes
      s1 e = fromJust $ G.sourceOf g e                     -- obtain source of e in R
      t1 e = fromJust $ G.targetOf g e                     -- obtain target of e in R
      s2 e = fromJust $ GM.applyNode mp (s1 e)             -- obtain source of m'(e) in G'
      t2 e = fromJust $ GM.applyNode mp (t1 e)             -- obtain target of m'(e) in G'
      tp e = fromJust $ GM.applyEdge typemor e             -- obtain type of e in R

      -- generate new edge table with new information
      edgeTable' = map (\(e,e2) -> (e, s1 e, t1 e, e2, s2 e, t2 e, tp e)) edgeTable

      -- create new morphism adding all edges
      kr''      = foldr (\(a,sa,ta,b,sb,tb,tp) tgm -> TGM.updateEdgeRelationTGM a b (TGM.createEdgeCodTGM b sb tb tp tgm) )
                        kr'
                        edgeTable'
  in (kr'', idMap kr'' k)


-- | Pushout
dpo :: TGM.TypedGraphMorphism a b -> GR.GraphRule a b
    -> (TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b, TGM.TypedGraphMorphism a b)
dpo m rule = 
  let
     l = GR.left rule
     r = GR.right rule
     (m', l') = poc m l
     (m'', r') = po m' r
  in (m',m'',l',r')

comatch (_,m'',_,_) = m''
