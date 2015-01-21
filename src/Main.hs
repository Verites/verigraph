module Main where

import Data.IORef
import qualified GraphGrammar as GG
import qualified GraphMorphism as GM
import qualified Graph as G
import GUI (createGUI, addMainCallbacks, NodePayload, EdgePayload, showGUI)
import Graphics.UI.Gtk

main = do
    initGUI
    let grammar :: GG.GraphGrammar NodePayload EdgePayload
        grammar = GG.graphGrammar (GM.empty G.empty G.empty) G.empty []
    gramRef <- newIORef grammar
    gui <- createGUI
    showGUI gui
    addMainCallbacks gui gramRef
    mainGUI 

