import Control.Monad
import Graph (Graph)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified TypedGraphMorphism as TM
import GraphRule
import Match

l = G.insertEdge 1 1 2 $
    G.insertEdge 2 3 3 $
    G.insertNode 1 $
	G.insertNode 2 $
	G.insertNode 3 $
	G.insertNode 4 $
    G.empty :: Graph String String

r = G.insertEdge 15 12 13 $
	G.insertEdge 16 14 14 $
	G.insertEdge 17 13 12 $
	G.insertEdge 18 10 15 $
	G.insertEdge 19 10 15 $
    G.insertNode 10 $
    G.insertNode 11 $
	G.insertNode 12 $
	G.insertNode 13 $
	G.insertNode 14 $
	G.insertNode 15 $
	G.insertNode 16 $
    G.empty :: Graph String String

t = G.insertEdge 1 1 2 $
    G.insertNode 1 $
    G.insertNode 2 $
    G.empty :: Graph String String

tl = GM.updateEdges 1 1 $
     GM.updateEdges 2 1 $
     GM.updateNodes 1 1 $
     GM.updateNodes 2 1 $
     GM.updateNodes 3 1 $
     GM.updateNodes 4 2 $
     GM.empty l t

tr = GM.updateEdges 15 1 $
     GM.updateEdges 16 1 $
     GM.updateEdges 17 1 $
     GM.updateEdges 18 1 $
     GM.updateEdges 19 1 $
     GM.updateNodes 10 1 $
     GM.updateNodes 11 2 $
     GM.updateNodes 12 1 $
     GM.updateNodes 13 2 $
     GM.updateNodes 14 1 $
     GM.updateNodes 15 1 $
     GM.updateNodes 16 1 $
     GM.empty r t

emptyRule =
    graphRule (TM.typedMorphism tl tl tl) (TM.typedMorphism tr tr tr) []
    
matches = findMatches emptyRule Mono tl tr
      
{-

r = G.Morphism [(Just (ln!!0), Nothing)] []
emptyRule = G.Morphism [] []

maps = findMatches Mono tdl tdg

main = do
	putStrLn $ "found " ++ (show (length maps)) ++ " mappings\n"
	forM_ [0 .. ((length maps) - 1)] $ \m ->
		let contents = "digraph G {\n" ++
				mappingToG.t (maps!!m) tdl tdg ++ "}"
		in writeFile ("graph" ++ (show m) ++ ".gv") contents
	--putStr $ graphToG.t tdl
	
-}
