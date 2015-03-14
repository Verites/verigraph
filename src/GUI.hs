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

type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload
type GraphMorphism = GM.GraphMorphism NodePayload EdgePayload
type Rule  = GR.GraphRule NodePayload EdgePayload
type Coords = (Double, Double)
type NodePayload = (Coords, Kolor)
type EdgePayload = Kolor

iGraphIdx = 0
tGraphIdx = 1
rulesIdx = 2

data GraphRel = GraphRel { getGraph :: Graph,
                           getNodeRelation :: R.Relation G.NodeId,
                           getEdgeRelation :: R.Relation G.EdgeId
                         }
data TreeNode = TNInitialGraph NodeStatus GraphRel | 
                TNTypeGraph NodeStatus Graph |
                TNRule NodeStatus String GraphRel |
                TNRoot String

data NodeStatus = Active | Inactive

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


runGUI :: IO ()
runGUI = do
--    gramRef <- newIORef grammar
    gui <- createGUI
    showGUI gui
--    addMainCallbacks gui
    return ()

showGUI = widgetShowAll . mainWindow

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


openFile :: IO ()
openFile = do
    dial <- fileChooserDialogNew (Just "Open") Nothing FileChooserActionOpen
                                 buttons
    file <- dialogRun dial
    putStrLn "Opening File"
  where
    buttons = [ ("Cancel", ResponseCancel), ("Open", ResponseOk) ]

createViewAndModel :: IO TreeView
createViewAndModel = do
--    tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
    store <- grammarToModel testGrammar
    
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
    cellLayoutSetAttributes col renderer store $ \row -> [ cellText := getName row ]

    treeViewSetModel view store
--    treeViewColumnAddAttribute col renderer "text" 0

    view `on` rowActivated $ rowSelected store
--    view `on` rowActivated $ rowSelected tree
--    view `on` rowActivated $ editIGraph tree path col
    return view
  where
    grammar :: GG.GraphGrammar NodePayload EdgePayload
    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []
    getName (TNInitialGraph _ _) = "Initial Graph"
    getName (TNTypeGraph _ _) = "Type Graph"
    getName (TNRoot _) = "Rules"
    getName (TNRule _ _ _) = "Rule"

rowSelected store path _ = do
    node <- treeStoreLookup store path
    case node of
        Nothing -> return ()
        Just n -> putStrLn . getName . T.rootLabel $ n
    val <- getTypeGraph store
    putStrLn "extracted"
        --(editIGraph (T.rootLabel n))
  where
    getName (TNInitialGraph _ _) = "Initial Graph"
    getName (TNTypeGraph _ _) = "Type Graph"
    getName (TNRule _ _ _) = "Rule"
    getName (TNRoot _) = "Rules"

getTypeGraph :: TreeStore a -> IO TreeNode
getTypeGraph store = do
    Just iter <- treeModelGetIterFirst store
    val  <- treeModelGetValue store iter $ makeColumnIdInt 0
    return val


grammarToModel :: Grammar -> IO (TreeStore TreeNode)
grammarToModel gg = do
    tree <- treeStoreNew [] :: IO (TreeStore TreeNode)
    treeStoreInsert tree [] 0 iGraphRel
    treeStoreInsert tree [] 1 tGraph
--    treeStoreInsertTree tree [] 2 ruleTree
    return tree
  where
    iGraph    = GG.initialGraph gg
    iNodeRel  = GM.nodeRelation iGraph
    iEdgeRel  = GM.edgeRelation iGraph
    iGraphRel = TNInitialGraph Active $ GraphRel (M.domain iGraph) iNodeRel iEdgeRel
    tGraph    = TNTypeGraph Active $ GG.typeGraph gg
{-
    ruleTree  = T.Node TNRoot ruleForest
    ruleForest = foldr (\(s, r) acc -> (T.Node (TNRule s r) []) : acc) [] rules
    rules  = GG.rules gg
-}

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

