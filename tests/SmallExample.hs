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

r = G.insertEdge 1 1 1 $
	G.insertEdge 2 1 2 $
    G.insertNode 1 $
	G.insertNode 2 $
	G.insertNode 3 $
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

tr = GM.updateEdges 1 1 $
     GM.updateEdges 2 1 $
     GM.updateNodes 1 1 $
     GM.updateNodes 2 1 $
     GM.updateNodes 3 2 $
     GM.empty r t

emptyRule =
    graphRule (TM.typedMorphism tl tl (id tl))
              (TM.typedMorphism tl tl (id tl))
              []
    
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
