 module GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import qualified GraphRule as GR
--import qualified TypedGraphMorphism as TM
import Data.Colour.Names
import Data.Colour.SRGB (Colour, toSRGB, RGB (..))
import Data.Colour.Palette.ColorSet (Kolor, webColors, infiniteWebColors)
import qualified Data.Foldable as F
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
data TreeNode = TNInitialGraph NodeStatus String GraphRel | 
                TNTypeGraph NodeStatus String Graph |
                TNRule NodeStatus String GraphRel |
                TNRoot String

instance Show TreeNode where
    show (TNInitialGraph _ s _) = s
    show (TNTypeGraph _ s _) = s
    show (TNRule _ s _) = s
    show (TNRoot s) = s


data NodeStatus = Active | Inactive

data GUI = GUI {
    treeView :: TreeView,
    mainWindow    :: Window,
    buttons :: Buttons,
    getCanvas :: DrawingArea
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
    addMainCallbacks gui
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
    
    canvas <- drawingAreaNew
    view <- createViewAndModel canvas
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

    let buttons = Buttons iGraphButton addRuleButton okButton
    return $ GUI view window buttons canvas 

addMainCallbacks :: GUI -> IO ()
addMainCallbacks gui = do
    let window   = mainWindow gui
        view = treeView gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
        okButton = getOkButton bs
    window `on` objectDestroy $ mainQuit

--    viewRef <- newIORef view
 --   gram <- readIORef gramRef
--    iGraphButton `on` buttonActivated $ iGraphDialog view
--    addRuleButton `on` buttonActivated $ ruleDialog gramRef
--    okButton `on` buttonActivated $ updateModel gram viewRef
    return ()



openFile :: IO ()
openFile = do
    dial <- fileChooserDialogNew (Just "Open") Nothing FileChooserActionOpen
                                 buttons
    file <- dialogRun dial
    putStrLn "Opening File"
  where
    buttons = [ ("Cancel", ResponseCancel), ("Open", ResponseOk) ]

createViewAndModel :: DrawingArea -> IO TreeView
createViewAndModel canvas = do
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

    view `on` rowActivated $ rowSelected canvas store
--    view `on` rowActivated $ rowSelected tree
--    view `on` rowActivated $ editIGraph tree path col
    return view
  where
    grammar :: GG.GraphGrammar NodePayload EdgePayload
    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []
    getName (TNInitialGraph _ s _) = s
    getName (TNTypeGraph _ s _) = s
    getName (TNRoot s) = s
    getName (TNRule _ s _) = s

rowSelected canvas store path _ = do
    tree <- treeStoreGetTree store [1]
    let tGraphs = getActiveTypeGraphs tree
    node <- treeStoreLookup store path
    case node of
        Nothing -> return ()
        Just (T.Node (TNInitialGraph _ _ g) _) -> editGraphRel canvas (head tGraphs) g
        otherwise -> putStrLn "was anderes"
--    editIGraph (T.rootLabel n))
    return ()

editGraphRel :: DrawingArea -> Graph -> GraphRel -> IO ()
editGraphRel canvas tGraph gRel@(GraphRel graph nR eR) =
    putStrLn "hallo"
{-
do
    let grBox = EditingBox
                (GM.graphMorphism graph tGraph nR eR) NoEditing
    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog >>= return . castToBox

    frame <- frameNew
    frameSetLabel frame "Initial Graph"
    canvas <- drawingAreaNew
    containerAdd frame canvas
    grBoxRef <- newIORef grBox
    tFrame  <- frameNew
    frameSetLabel tFrame "T Graph"
    tCanvas <- drawingAreaNew
    containerAdd tFrame tCanvas

    canvas `on` buttonPressEvent $ mouseClick canvas tCanvas grBoxRef domClick
    canvas `on` draw $ updateCanvas grBoxRef M.domain
    widgetAddEvents canvas [Button3MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas grBoxRef

    
    tCanvas `on` buttonPressEvent $ mouseClick canvas tCanvas grBoxRef codClick
    tCanvas `on` draw $ updateCanvas grBoxRef M.codomain
    widgetAddEvents tCanvas [Button3MotionMask]
    tCanvas `on` motionNotifyEvent $ mouseMove tCanvas grBoxRef

    boxPackStart contentArea frame PackGrow 1
    boxPackStart contentArea tFrame PackGrow 1
    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Apply" ResponseApply

    widgetSetSizeRequest dialog 800 600
    widgetShowAll dialog
    response <- dialogRun dialog
    morph <- readIORef grBoxRef >>= return . mapEdges . eBoxGraphMorphism
--    let gram' = GG.graphGrammar morph (GG.rules gram)
    case response of
        ResponseApply | valid morph -> do
--                           writeIORef gramRef gram'
                           putStrLn $ "morphism valid"
                           widgetDestroy dialog
                      | otherwise -> putStrLn $ "morphism invalid"
        otherwise -> do putStrLn $ "changes to initial graph cancelled"
                        widgetDestroy dialog
    return ()
-}


getActiveTypeGraphs :: T.Tree TreeNode -> [Graph]
getActiveTypeGraphs tree =
    F.foldr (\n acc -> case n of
                          TNTypeGraph Active _ g -> g : acc
                          otherwise -> acc)
            [] tree


grammarToModel :: Grammar -> IO (TreeStore TreeNode)
grammarToModel gg = do
    tree <- treeStoreNew [] :: IO (TreeStore TreeNode)
    treeStoreInsert tree [] 0 iGraphRel
    treeStoreInsert tree [] 1 $ TNRoot "Type Graphs"
--    treeStoreInsert tree [] 2 $ TNRoot "Rules"
    treeStoreInsertTree tree [] 2 ruleTree
    treeStoreInsert tree [1] 0 tGraph
    return tree
  where
    iGraph    = GG.initialGraph gg
    iNodeRel  = GM.nodeRelation iGraph
    iEdgeRel  = GM.edgeRelation iGraph
    iGraphRel = TNInitialGraph Active "G0" $
        GraphRel (M.domain iGraph) iNodeRel iEdgeRel
    tGraph    = TNTypeGraph Active "t0" $ GG.typeGraph gg
    ruleTree  = T.Node (TNRoot "Rules") ruleForest
    ruleForest = foldr (\(s, r) acc ->
        (T.Node (TNRule Active s (ruleToGraphRel r)) []) : acc) [] rules
    ruleToGraphRel r = GraphRel (M.domain dom)
                                nodeRel
                                edgeRel
        where
            dom = M.domain . GR.left $ r
            nodeRel = GM.nodeRelation dom
            edgeRel = GM.edgeRelation dom
    rules  = GG.rules gg

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

