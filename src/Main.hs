module Main where

--import GUI (createGUI, addMainCallbacks, NodePayload, EdgePayload, showGUI)
import GUI (runGUI)
import Graphics.UI.Gtk

main = do
    initGUI
    runGUI
    mainGUI 

