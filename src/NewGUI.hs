module GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import qualified GraphRule as GR
import Data.Colour.Names
import Data.Colour.SRGB (Colour, toSRGB, RGB (..))
import Data.Colour.Palette.ColorSet (Kolor, webColors, infiniteWebColors)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.IORef
import qualified Data.Tree as T
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Data.Foldable (mapM_)
import Data.Traversable (sequenceA, traverse)
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Gtk
import Graphics.UI.Gtk.Gdk.EventM
import qualified Morphism as M
import Prelude hiding (mapM_, any)
import qualified Relation as R
import Valid (valid)

type Coords = (Double, Double)
type NodePayload = (Coords, Kolor)
type EdgePayload = Kolor
type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload
type GraphMorphism = GM.GraphMorphism NodePayload EdgePayload
type Rule  = GR.GraphRule NodePayload EdgePayload


data GraphRel = GraphRel { getGraph :: Graph,
                           getNodeRel :: M.Map G.NodeId G.NodeId,
                           getEdgeRel :: M.Map G.EdgeId G.EdgeId
                         } 

data GrammarTree = GTGraph     { getInitialGraph :: GraphRel } |
                   GTTypeGraph { getTypeGraph :: Graph } |
                   GTRule      { getRule :: GraphRel }

data GUI = GUI {
    treeView :: TreeView,
    mainWindow    :: Window,
    buttons :: Buttons,
    canvas :: DrawingArea
    }

data Buttons = Buttons {
    editInitialGraph :: Button,
    addRule :: Button,
    getOkButton :: Button
    }


createGUI :: IO GUI
createGUI = do
    window <- windowNew
    set window [ windowTitle := "Verigraph" ]
    mainVBox <- vBoxNew False 1
    hBox <- hBoxNew False 1
    vBox0 <- vBoxNew False 1
    vBox1 <- vBoxNew False 1
    containerAdd window mainVBox

    -- Menu Widgets
    menuBar <- menuBarNew
    fileMenuItem <- menuItemNewWithLabel "File"
    fileMenu <- menuNew
    menuShellAppend menuBar fileMenuItem
    menuItemSetSubmenu fileMenuItem fileMenu
    newItem <- menuItemNewWithLabel "New"
    openItem <- menuItemNewWithLabel "Open"
    saveItem <- menuItemNewWithLabel "Save"
    menuAttach fileMenu newItem 0 1 0 1 
    menuAttach fileMenu openItem 0 1 1 2 
    menuAttach fileMenu saveItem 0 1 2 3 

    openItem `on` menuItemActivated $ openFile
    
    view <- createViewAndModel
    boxPackStart mainVBox menuBar PackNatural 1
    boxPackStart mainVBox hBox PackGrow 1
    boxPackStart hBox vBox0 PackNatural 1
    boxPackStart hBox vBox1 PackNatural 1

    boxPackStart vBox0 view PackGrow 1

    iGraphButton <- buttonNewWithLabel "Edit initial graph"
    addRuleButton <- buttonNewWithLabel "Add rule"
    okButton <- buttonNewWithLabel "OK"
    boxPackStart vBox1 iGraphButton PackNatural 1
    boxPackStart vBox1 addRuleButton PackNatural 1
    boxPackStart vBox1 okButton PackNatural 1
    dummyCanvas <- drawingAreaNew

    let buttons = Buttons iGraphButton addRuleButton okButton
    return $ GUI view window buttons dummyCanvas 


createViewAndModel :: IO TreeView
createViewAndModel = do
--    tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
    tree <- grammarToModel testGrammar
    
{-
    treeStoreInsert tree [] 0 "Graph"
    treeStoreInsert tree [] 1 "TGraph"
    treeStoreInsertTree tree [] 2 ruleTree
-}
--    treeStoreInsert tree [2] 0 ruleTree

    view <- treeViewNew
    col  <- treeViewColumnNew

    treeViewColumnSetTitle col "Graph Grammar"
    treeViewAppendColumn view col
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
--    cellLayoutSetAttributes col renderer tree $ \row -> [ cellText := row ]
    cellLayoutSetAttributes col renderer tree $ \row -> [ cellText := getName row ]

    treeViewSetModel view tree
--    treeViewColumnAddAttribute col renderer "text" 0

--    view `on` rowActivated $ rowSelected tree
--    view `on` rowActivated $ editIGraph tree path col
    return view
  where
    grammar :: GG.GraphGrammar NodePayload EdgePayload
    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []
    getName (GTGraph _) = "Initial Graph"
    getName (GTTypeGraph _) = "Type Graph"
--    getName GTrNode = "Rules"
    getName (GTRule _) = "Rule"

testGrammar :: Grammar
testGrammar =
    GG.graphGrammar iGraph []
  where
    iGraph = GM.graphMorphism g t nR eR
    g = G.empty :: Graph
    t = G.empty :: Graph
{-
    g = G.insertNodeWithPayload 0 ((100, 100), neutralColor) $
        G.insertNodeWithPayload 1 ((100, 150), neutralColor) $
        G.empty
    t = G.empty
-}
    nR = R.empty [] []
    eR = R.empty [] []


grammarToModel :: Grammar -> IO (TreeStore GrammarTree)
grammarToModel gg = do
    tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
    return tree

{-
    treeStoreInsert tree [] 0 iGraph
    treeStoreInsert tree [] 1 tGraph
    treeStoreInsertTree tree [] 2 ruleTree
    return tree
  where
    iGraph = GTGraph $ GG.initialGraph gg
    tGraph = GTTypeGraph $ GG.typeGraph gg
    ruleTree = T.Node GTrNode ruleForest
    ruleForest = foldr (\r acc -> (T.Node (GTrRule r) []) : acc) [] rules
    rules  = GG.rules gg
-}


{-
modelToGrammar :: TreeStore GrammarTree -> IO (Maybe Grammar)
modelToGrammar tree = do
    iGraphM <- treeStoreLookup tree [0]
    tGraphM <- treeStoreLookup tree [1]
    rulesM  <- treeStoreLookup tree [2]
    let elems = do iGraphNode <- iGraphM
                   rulesNode  <- rulesM
                   let rules = map (getRule . T.rootLabel)
                                   (T.subForest rulesNode)
                   return $ GG.graphGrammar (getIGraph $ T.rootLabel iGraphNode)
                                            rules
    return elems
-}




openFile :: IO ()
openFile = do
    dial <- fileChooserDialogNew (Just "Open") Nothing FileChooserActionOpen
                                 buttons
    file <- dialogRun dial
    putStrLn "Opening File"
  where
    buttons = [ ("Cancel", ResponseCancel), ("Open", ResponseOk) ]


showGUI = widgetShowAll . mainWindow

addMainCallbacks :: GUI -> IO ()
addMainCallbacks gui = do
    let window   = mainWindow gui
        view = treeView gui
        bs = buttons gui
--        iGraphButton = editInitialGraph bs
--        addRuleButton = addRule bs
        okButton = getOkButton bs
    window `on` objectDestroy $ mainQuit
    viewRef <- newIORef view
 --   gram <- readIORef gramRef
--    iGraphButton `on` buttonActivated $ iGraphDialog view
--    addRuleButton `on` buttonActivated $ ruleDialog gramRef
--    okButton `on` buttonActivated $ updateModel gram viewRef
    return ()


runGUI :: IO ()
runGUI = do
    gui <- createGUI
    showGUI gui
    addMainCallbacks gui
    return ()




