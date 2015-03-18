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
type DrawingFunc = Coords -> Render ()
type NodePayload = (Coords, DrawingFunc)
type EdgePayload = Kolor
data Obj = Node Int | Edge Int
    deriving (Show, Eq)

iGraphIdx = 0
tGraphIdx = 1
rulesIdx = 2
defRadius = 20 :: Double
defLineWidth = 2 :: Double
defBorderColor = black
defSpacing = 1
neutralColor = gray
renderColor :: Kolor -> Gtk.Render ()
renderColor k = setSourceRGB r g b
  where
    rgb = toSRGB k
    (r, g, b) = (channelRed rgb, channelGreen rgb, channelBlue rgb)



{- Data types for rendering -}
data RNode = RNode Graph G.NodeId
data REdge = REdge Graph G.EdgeId
data RGraph = RGraph Graph

class Renderable a where
    render :: a -> Render ()

instance Renderable (RNode) where
    render (RNode g n) =
        case G.nodePayload g n of
            Just (c, f) -> f c
            otherwise   -> return ()


instance Renderable (RGraph) where
    render (RGraph g) = do
        mapM_ (render . RNode g) $ G.nodes g
--        mapM_ (render . REdge g) $ G.edges g


drawCircle :: Kolor -> Coords -> Render ()
drawCircle color (x, y) = do
    setLineWidth defLineWidth
    renderColor defBorderColor
    Gtk.arc x y defRadius 0 $ 2 * pi
    strokePreserve
    renderColor color
    fill


data CanvasMode = IGraphMode |
                  TGraphMode String |
                  RuleMode String
    deriving Show

data State = State {
    canvasMode :: CanvasMode,
    getInitialGraph :: GraphRel,
    getTypeGraphs   :: M.Map String (NodeStatus, Graph),
    getRules        :: M.Map String (NodeStatus, GraphRel)
    }

data GraphRel = GraphRel { getGraph :: Graph,
                           getNodeRelation :: R.Relation G.NodeId,
                           getEdgeRelation :: R.Relation G.EdgeId
                         }
data TreeNode = TNInitialGraph NodeStatus String | 
                TNTypeGraph NodeStatus String |
                TNRule NodeStatus String |
                TNRoot String

instance Show TreeNode where
    show (TNInitialGraph _ s ) = s
    show (TNTypeGraph _ s) = s
    show (TNRule _ s) = s
    show (TNRoot s) = s


data NodeStatus = Active | Inactive

data GUI = GUI {
    treeStore :: TreeStore TreeNode,
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
    let state = grammarToState testGrammar
    stateRef <- newIORef state
    gui <- createGUI state
    showGUI gui
    addMainCallbacks gui stateRef
    return ()

showGUI = widgetShowAll . mainWindow

createGUI :: State -> IO GUI
createGUI state = do
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
    store <- createModel state
    view <- createView store
    boxPackStart mainVBox menuBar PackNatural 1
    boxPackStart mainVBox hBox PackGrow 1
    boxPackStart hBox vBox0 PackNatural 1
    boxPackStart hBox vBox1 PackGrow 1

    boxPackStart vBox0 view PackGrow 1

    iGraphButton <- buttonNewWithLabel "Edit initial graph"
    addRuleButton <- buttonNewWithLabel "Add rule"
    okButton <- buttonNewWithLabel "OK"
{-
    boxPackStart vBox1 iGraphButton PackNatural 1
    boxPackStart vBox1 addRuleButton PackNatural 1
    boxPackStart vBox1 okButton PackNatural 1
-}
    boxPackStart vBox1 canvas PackGrow 1

    let buttons = Buttons iGraphButton addRuleButton okButton
    return $ GUI store view window buttons canvas 

addMainCallbacks :: GUI -> IORef State -> IO ()
addMainCallbacks gui stateRef = do
    let window   = mainWindow gui
        view = treeView gui
        store = treeStore gui
        canvas = getCanvas gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
        okButton = getOkButton bs
    window `on` objectDestroy $ mainQuit
    canvas `on` buttonPressEvent $ mouseClick canvas stateRef
    canvas `on` draw $ updateCanvas stateRef
    view `on` rowActivated $ rowSelected gui store stateRef
--    viewRef <- newIORef view
 --   gram <- readIORef gramRef
--    iGraphButton `on` buttonActivated $ iGraphDialog view
--    addRuleButton `on` buttonActivated $ ruleDialog gramRef
--    okButton `on` buttonActivated $ updateModel gram viewRef
    return ()

rowSelected gui store stateRef path _ = do
--    tree <- treeStoreGetTree store [1]
--    Just model <- treeViewGetModel view
--    Just iter  <- treeModelGetIterFirst model
    state <- readIORef stateRef
    let tGraphs = getActiveTypeGraphs state
    node <- treeStoreLookup store path

--        Just (T.Node (TNInitialGraph _ _ g) _) -> editGraphRel (head tGraphs) g
--        Just n -> processClicked stateRef
    let state' = case node of
                    Just (T.Node (TNInitialGraph _ _) _) ->
                        state { canvasMode = IGraphMode }
                    Just (T.Node (TNTypeGraph _ s) _) ->
                        state { canvasMode = TGraphMode s }
                    Just (T.Node (TNRule _ s) _) ->
                        state { canvasMode = RuleMode s }
                    otherwise -> state
--        otherwise -> putStrLn "was anderes"

--    editIGraph (T.rootLabel n))
    writeIORef stateRef state'
    widgetQueueDraw $ getCanvas gui
    return ()

mouseClick :: WidgetClass widget =>
    widget -> IORef State -> EventM EButton Bool
mouseClick widget stateRef = do
    coords <- eventCoordinates
    button <- eventButton
    click <- eventClick
    state <- liftIO $ readIORef stateRef
        
    return True                

{-
fetchObj :: Graph -> Coords -> Maybe Obj
fetchObj graph coords
    | not . null $ fetchNodes = Just . Node . head $ fetchNodes
--    | not . null $ fetchEdges = Just . Node . head $ fetchEdges
    | otherwise = Nothing
  where
    fetchNodes = filter (overNode graph coords) $ G.nodes graph
-}


updateCanvas :: IORef State -> Render ()
updateCanvas stateRef = do
    state <- liftIO $ readIORef stateRef
    case canvasMode state of
        IGraphMode -> do --render . RGraph . getGraph . getInitialGraph $ state
            renderColor orange
            Gtk.arc 100 40 defRadius 0 $ 2 * pi
            fill
            return ()
        TGraphMode _ -> do
            renderColor green
            Gtk.arc 100 40 defRadius 0 $ 2 * pi
            fill
            return ()
        otherwise  -> do
            renderColor red
            Gtk.arc 100 40 defRadius 0 $ 2 * pi
            fill
            return ()
      

openFile :: IO ()
openFile = do
    dial <- fileChooserDialogNew (Just "Open") Nothing FileChooserActionOpen
                                 buttons
    file <- dialogRun dial
    putStrLn "Opening File"
  where
    buttons = [ ("Cancel", ResponseCancel), ("Open", ResponseOk) ]

createModel :: State -> IO (TreeStore TreeNode)
createModel state = stateToModel state

createView :: TreeStore TreeNode -> IO TreeView
createView store = do
--    tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
--    let state = grammarToState testGrammar
--    stateRef <- newIORef state
--    store <- stateToModel state
    
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

--    view `on` rowActivated $ rowSelected stateRef
--    view `on` rowActivated $ rowSelected tree
--    view `on` rowActivated $ editIGraph tree path col
    return view
  where
--    grammar :: GG.GraphGrammar NodePayload EdgePayload
--    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []
    getName (TNInitialGraph _ s) = s
    getName (TNTypeGraph _ s) = s
    getName (TNRoot s) = s
    getName (TNRule _ s) = s


editGraphRel :: Graph -> GraphRel -> IO ()
editGraphRel tGraph gRel@(GraphRel graph nR eR) = putStrLn "editing"

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


getActiveTypeGraphs :: State -> [Graph]
getActiveTypeGraphs state =
    map (\(s, g) -> g) $ M.elems $ M.filter active $ getTypeGraphs state
  where
    active (Active, _) = True
    active _ = False

grammarToState :: Grammar -> State
grammarToState gg = State IGraphMode iGraphRel tGraphMap rulesMap
  where
    iGraph    = GG.initialGraph gg
    iNodeRel  = GM.nodeRelation iGraph
    iEdgeRel  = GM.edgeRelation iGraph
    iGraphRel = GraphRel (M.domain iGraph) iNodeRel iEdgeRel
    tGraph = (Active, GG.typeGraph gg)
    tGraphMap = M.insert "t0" tGraph $ M.empty
    rulesMap = M.empty
--    rules  = GG.rules gg
--    ruleToGraphRel 
--    rulesMap = foldr (\(s, r) acc -> M.insert s r acc) M.empty rules

stateToModel :: State -> IO (TreeStore TreeNode)
stateToModel state = do
    tree <- treeStoreNew [] :: IO (TreeStore TreeNode)
    treeStoreInsert tree [] 0 $ TNInitialGraph Active "G0"
    treeStoreInsert tree [] 1 $ TNRoot "Type Graphs"
    treeStoreInsertForest tree [1] 0 tGraphForest
--    treeStoreInsertTree tree [] 2 ruleTree
    return tree
  where
    rules   = getRules state
    tGraphs = M.toList $ getTypeGraphs state
    tGraphForest = map (\(s, g) -> T.Node (TNTypeGraph Active s) []) tGraphs
    

grammarToModel :: Grammar -> IO (TreeStore TreeNode)
grammarToModel = stateToModel . grammarToState
{-
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



