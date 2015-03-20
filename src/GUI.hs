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
import Data.List.Utils
import Data.Maybe (fromJust, isJust, isNothing)
import Data.IORef
import Debug.Trace
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
type NodePayload = (Coords, Coords -> Render (), Coords -> Coords -> Bool)
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
            Just (coords, renderFunc, checkFunc) -> renderFunc coords
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

insideCircle :: Double -> Coords -> Coords -> Bool
insideCircle radius circleCoords coords =
    norm circleCoords coords <= radius

norm :: Coords -> Coords -> Double
norm (x, y) (x', y') =
    sqrt $ (square (x' - x)) + (square (y' - y))
  where
    square x = x * x

data CanvasMode = IGraphMode Key |
                  TGraphMode Key |
                  RuleMode Key
    deriving Show


data SelMode = SelNodes [G.NodeId]
             | SelEdges [G.EdgeId]
             | DragNodes [G.NodeId]
             | DrawEdge G.NodeId
             | IdleMode
    

data RowStatus = Active | Inactive

type GrammarEntry = (RowStatus, SelMode, GraphRel)
type Key = String

-- To keep it uniform, typegraphs are also described as GraphRel
data State =
    State { canvasMode :: CanvasMode,
            getInitialGraphs :: [(Key, GrammarEntry)],
            getTypeGraphs   :: [(Key, GrammarEntry)],
            getRules        :: [(Key, GrammarEntry)]
          }

data GraphRel = GraphRel { getGraph :: Graph,
                           getNodeRelation :: R.Relation G.NodeId,
                           getEdgeRelation :: R.Relation G.EdgeId
                         } deriving Show
data TreeNode = TNInitialGraph RowStatus Key | 
                TNTypeGraph RowStatus Key |
                TNRule RowStatus Key |
                TNRoot Key

instance Show TreeNode where
    show (TNInitialGraph _ s ) = s
    show (TNTypeGraph _ s) = s
    show (TNRule _ s) = s
    show (TNRoot s) = s



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
--    okButton `on` buttonActivated $ updateModel gram viewRef
    return ()

rowSelected gui store stateRef path _ = do
    state <- readIORef stateRef
    let tGraphs = getActiveTypeGraphs state
    node <- treeStoreLookup store path

    let state' = case node of
                    Just (T.Node (TNInitialGraph _ s) _) ->
                        state { canvasMode = IGraphMode s }
                    Just (T.Node (TNTypeGraph _ s) _) ->
                        state { canvasMode = TGraphMode s }
                    Just (T.Node (TNRule _ s) _) ->
                        state { canvasMode = RuleMode s }
                    otherwise -> state

    writeIORef stateRef state'
    widgetQueueDraw $ getCanvas gui
    return ()

mouseClick :: WidgetClass widget =>
    widget -> IORef State -> EventM EButton Bool
mouseClick canvas stateRef = do
    coords <- eventCoordinates
    button <- eventButton
    click <- eventClick
    state <- liftIO $ readIORef stateRef
    let Just grel = currentGraph state -- FIXME unsafe pattern matching
        grel' = processClick grel coords button click
    liftIO $ writeIORef stateRef $
        case canvasMode state of
            IGraphMode k -> state { getInitialGraphs =
                                        addToAL (getInitialGraphs state)
                                                k (Active, IdleMode, grel')
                                }
            TGraphMode k -> state { getTypeGraphs =
                                        addToAL (getTypeGraphs state)
                                                k (Active, IdleMode, grel') -- FIXME get status
                                  }
            otherwise -> state
    liftIO $ widgetQueueDraw canvas
    return True

processClick :: GraphRel -> Coords -> MouseButton -> Click -> GraphRel
processClick grel coords@(x, y) button click =
    case (objects, button, click) of
        ([], LeftButton, DoubleClick) -> grel { getGraph = addNode }
        otherwise -> grel
  where
    g = getGraph grel
    listPayloads = map (\n -> G.nodePayload g n) $ G.nodes g
    objects = filter (\p -> case p of
                                Just (refCoords, _ , cf) -> cf refCoords coords
                                otherwise -> False)
                     listPayloads
    addNode = newNode g coords (drawCircle neutralColor) (insideCircle defRadius)

newNode :: Graph -> Coords -> (Coords -> Render ())
                           -> (Coords -> Coords -> Bool) -> Graph
newNode graph coords renderFunc checkFunc  =
    G.insertNodeWithPayload newId (coords, renderFunc, checkFunc) graph
  where
    newId = length . G.nodes $ graph


currentGraph :: State -> Maybe GraphRel
currentGraph state =
    case canvasMode state of
        IGraphMode k  -> lookup k iGraphs >>= return . graphRel
        TGraphMode k -> lookup k tGraphs >>= return . graphRel
        RuleMode k   -> lookup k rules >>= return . graphRel
  where
    iGraphs = getInitialGraphs state
    tGraphs = getTypeGraphs state
    rules   = getRules state
    graphRel = \(_, _, grel) -> grel


updateCanvas :: IORef State -> Render ()
updateCanvas stateRef = do
    state <- liftIO $ readIORef stateRef
    case canvasMode state of
        IGraphMode k -> fetchAndRender k $ getInitialGraphs state
        TGraphMode k -> fetchAndRender k $ getTypeGraphs state
        otherwise  -> do
            renderColor red
            Gtk.arc 100 40 defRadius 0 $ 2 * pi
            fill
            return ()
  where
    graphRel = \(_, _, grel) -> grel
    fetchAndRender k l =
        let graph = lookup k l
        in case graph of
            Nothing -> return ()
            Just t  -> render . RGraph . getGraph . graphRel $ t
      

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

    view <- treeViewNew
    col  <- treeViewColumnNew

    treeViewColumnSetTitle col "Graph Grammar"
    treeViewAppendColumn view col
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer store $ \row -> [ cellText := getName row ]

    treeViewSetModel view store
--    treeViewColumnAddAttribute col renderer "text" 0

    return view
  where
    getName (TNInitialGraph _ s) = s
    getName (TNTypeGraph _ s) = s
    getName (TNRoot s) = s
    getName (TNRule _ s) = s


getActiveTypeGraphs :: State -> [GraphRel]
getActiveTypeGraphs state =
    map (\(_, (_, _, g)) -> g) $ filter active . getTypeGraphs $ state
  where
    active (_, (Active, _, _)) = True
    active _ = False

grammarToState :: Grammar -> State
grammarToState gg = State (IGraphMode "g0") iGraphList tGraphList rulesList
  where
    iGraph    = GG.initialGraph gg
    iNodeRel  = GM.nodeRelation iGraph
    iEdgeRel  = GM.edgeRelation iGraph
    iGraphRel = GraphRel (M.domain iGraph) iNodeRel iEdgeRel
    emptyRel = R.empty [] []
    tGraph = (Active, IdleMode, GraphRel (GG.typeGraph gg) emptyRel emptyRel)
    tGraphList = addToAL [] "t0" tGraph
    iGraphList = [("g0", (Active, IdleMode, iGraphRel))]
    rulesList = []
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
    tGraphs = getTypeGraphs state
    tGraphForest = map (\(s, g) -> T.Node (TNTypeGraph Active s) []) tGraphs
    

grammarToModel :: Grammar -> IO (TreeStore TreeNode)
grammarToModel = stateToModel . grammarToState

testGrammar :: Grammar
testGrammar =
    GG.graphGrammar iGraph []
  where
    iGraph = GM.graphMorphism g t nR eR
    g = G.empty :: Graph
    t = G.empty :: Graph
    nR = R.empty [] []
    eR = R.empty [] []
