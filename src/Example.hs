import Control.Monad
import Graph (Graph)
import qualified GraphClass as G
--import Assorted.Render
import Match

l = G.insertEdge 1 1 2 $
    G.insertEdge 2 3 3 $
    G.insertNode 1 $
    G.insertNode 2 $
    G.insertNode 3 $
    G.insertNode 4 $
    G.empty :: Graph Int Int

g = G.insertEdge 17 14 14 $
	G.insertEdge 16 12 14 $
	G.insertEdge 15 10 11 $ 
	G.insertNode 10 $
	G.insertNode 11 $
	G.insertNode 12 $
    G.insertNode 13 $
    G.insertNode 14 $
    G.empty :: Graph Int Int

maps = findMatches Mono l g

{-
main = do
    putStrLn $ "found " ++ (show (length maps)) ++ " mappings\n"
    forM_ [0 .. ((length maps) - 1)] $ \m ->
        let contents = "digraph G {\n" ++
                mappingToDot (maps!!m) tdl tdg ++ "}"
        in writeFile ("graph" ++ (show m) ++ ".gv") contents
    --putStr $ graphToDot tdl
-}
