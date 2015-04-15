{-# LANGUAGE TemplateHaskell #-} -- fclabels

module GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import qualified GraphRule as GR
--import qualified TypedGraphMorphism as TM
--import Data.Colour.Names
--import Data.Colour.SRGB (Colour, toSRGB, RGB (..))
import qualified Data.Foldable as F
import Data.Label -- fclabels
import Data.List.Utils
import Data.Maybe (fromMaybe)
import Data.IORef
import Debug.Trace
import qualified Data.Tree as T
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Data.Foldable (mapM_)
import Data.Traversable (sequenceA, traverse)
import Graphics.UI.Gtk hiding (get, set) -- conflict with fclabels
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Gtk
import Graphics.UI.Gtk.Gdk.EventM
import qualified Morphism as M
import Prelude hiding (mapM_, any, (.), id)
import Control.Category -- for fclabels, including (.) and id
import qualified Relation as R
import Valid (valid)

type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload
type GraphMorphism = GM.GraphMorphism NodePayload EdgePayload
type Rule = GR.GraphRule NodePayload EdgePayload
type Coords = (Double, Double)
type NodePayload =
    (Coords, Bool -> Coords -> Render (), Coords -> Coords -> Bool)
type EdgePayload = Color
data Obj = Node Int | Edge Int
    deriving (Show, Eq)

-- To keep it uniform, typegraphs are also described as GraphEditState
data State = State
    { _canvasMode       :: CanvasMode
    , _getInitialGraphs :: [(Key, GraphEditState)]
    , _getTypeGraph    :: GraphEditState
    , _getRules         :: [(Key, GraphEditState)]
    }

data GraphEditState = GraphEditState
    { _getStatus :: RowStatus
    , _getSelMode :: SelMode
    , _getGraph :: Graph
    , _getNodeRelation :: R.Relation G.NodeId
    , _getEdgeRelation :: R.Relation G.EdgeId
    } deriving Show

data CanvasMode =
      IGraphMode Key
    | TGraphMode
    | RuleMode Key
    deriving Show


data SelMode =
      SelNodes [G.NodeId]
    | SelEdges [G.EdgeId]
--  | DragNodes [G.NodeId]
    | DrawEdge G.NodeId
    | IdleMode
    deriving Show
    

data RowStatus = Active | Inactive
    deriving (Eq, Show)

type Key = String


data TreeNode
    = TNInitialGraph RowStatus Key
    | TNTypeGraph
    | TNRule RowStatus Key
    | TNRoot Key

instance Show TreeNode where
    show (TNInitialGraph _ s ) = s
    show TNTypeGraph = "Type Graph"
    show (TNRule _ s) = s
    show (TNRoot s) = s



data GUI = GUI
    { _treeStore :: TreeStore TreeNode
    , _treeView :: TreeView
    , _mainWindow  :: Window
    , _getCanvas :: DrawingArea
    }

data Buttons = Buttons
    { _editInitialGraph :: Button
    , _addRule :: Button
    , _getOkButton :: Button
    }

$(mkLabels [''State, ''GraphEditState, ''GUI, ''Buttons])


defGraphName = "g0"
defTGraphName = "t0"
defRadius = 20 :: Double
defLineWidth = 2 :: Double
-- FIXME temporary magic constants
defBorderColor = Color 65535 65535 65535
initialColor = Color 13363 25956 42148
defSpacing = 1
neutralColor = Color 13363 25956 42148 -- gainsboro
renderColor :: Color -> Gtk.Render ()
renderColor (Color r g b) = setSourceRGB  r' g' b'
  where
    (r', g', b') = (fromIntegral r / denom,
                    fromIntegral g / denom,
                    fromIntegral b / denom) :: (Double, Double, Double)
    denom = 65535 :: Double
--    rgb = toSRGB k
--    (r, g, b) = (channelRed rgb, channelGreen rgb, channelBlue rgb)

{- Data types for rendering -}
data RNode = RNode GraphEditState G.NodeId
data REdge = REdge Graph G.EdgeId
data RGraph = RGraph Graph

class Renderable a where
    render :: a -> Render ()

instance Renderable (RNode) where
    render (RNode gstate n) =
        case G.nodePayload g n of
            Just (coords, renderFunc, checkFunc) -> renderFunc sel coords
            otherwise -> return ()
      where
        g = _getGraph gstate
        sel = case _getSelMode gstate of
                SelNodes ns -> n `elem` ns
                otherwise -> False


instance Renderable GraphEditState where
    render gstate = do
        mapM_ (render . RNode gstate) $ G.nodes (_getGraph gstate)
        

drawCircle :: Color -> Bool -> Coords -> Render ()
drawCircle color sel (x, y) = do
    setLineWidth defLineWidth
    renderColor defBorderColor
    Gtk.arc x y defRadius 0 $ 2 * pi
    strokePreserve
    renderColor color
    if sel then fillPreserve >> highlight else fill
  where
    highlight = do
        setSourceRGBA 0 0 0 0.4
        fill

insideCircle :: Double -> Coords -> Coords -> Bool
insideCircle radius circleCoords coords =
    norm circleCoords coords <= radius

norm :: Coords -> Coords -> Double
norm (x, y) (x', y') =
    sqrt $ (square (x' - x)) + (square (y' - y))
  where
    square x = x * x


runGUI :: IO ()
runGUI = do
    let state = grammarToState testGrammar
    stateRef <- newIORef state
    gui <- createGUI state
    showGUI gui
    addMainCallbacks gui stateRef
    return ()

showGUI = widgetShowAll . _mainWindow

createGUI :: State -> IO GUI
createGUI state = do
    window <- windowNew
    Gtk.set window [ windowTitle := "Verigraph"
               , windowDefaultWidth := 800
               , windowDefaultHeight := 600 ]
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

    openItem `on` menuItemActivate $ openFileDialog
    
    canvas <- drawingAreaNew
    store <- createModel state
    view <- createView store
    boxPackStart mainVBox menuBar PackNatural 1
    boxPackStart mainVBox hBox PackGrow 1
    boxPackStart hBox vBox0 PackNatural 1
    boxPackStart hBox vBox1 PackGrow 1

    boxPackStart vBox0 view PackGrow 1

    boxPackStart vBox1 canvas PackGrow 1

    return $ GUI store view window canvas 

addMainCallbacks :: GUI -> IORef State -> IO ()
addMainCallbacks gui stateRef = do
    let window   = _mainWindow gui
        view = _treeView gui
        store = _treeStore gui
        canvas = _getCanvas gui
    window `on` objectDestroy $ mainQuit
    canvas `on` buttonPressEvent $ mouseClick canvas stateRef
    dwin <- widgetGetDrawWindow canvas
    canvas `on` exposeEvent $ do liftIO $ renderWithDrawable dwin (updateCanvas stateRef)
                                 return True
    view `on` cursorChanged $ rowSelected gui store stateRef view
    return ()

rowSelected gui store stateRef view = do
    (path, _) <- treeViewGetCursor view
    state <- readIORef stateRef
    let tGraph = _getTypeGraph state
    node <- treeStoreLookup store path

    let state' = case node of
                    Just (T.Node (TNInitialGraph _ s) _) ->
                        set canvasMode (IGraphMode s) state
                    Just (T.Node TNTypeGraph _) ->
                        set canvasMode TGraphMode state
                    Just (T.Node (TNRule _ s) _) ->
                        set canvasMode (RuleMode s) state
                    otherwise -> state

    writeIORef stateRef state'
    widgetQueueDraw $ _getCanvas gui
    return ()

mouseClick :: WidgetClass widget
           => widget -> IORef State -> EventM EButton Bool
mouseClick canvas stateRef = do
    coords <- eventCoordinates
    button <- eventButton
    click <- eventClick
    state <- liftIO $ readIORef stateRef
    let Just gstate = currentGraph state -- FIXME unsafe pattern matching
    gstate' <- liftIO $ chooseMouseAction state gstate coords button click
    liftIO $ writeIORef stateRef $
        case _canvasMode state of
            IGraphMode k ->
                modify getInitialGraphs
                       (\s -> addToAL s k gstate')
                       state
            TGraphMode ->
                set getTypeGraph gstate' state
            otherwise -> state
    liftIO $ widgetQueueDraw canvas
    return True

chooseMouseAction :: State
                  -> GraphEditState
                  -> Coords
                  -> MouseButton
                  -> Click
                  -> IO GraphEditState
chooseMouseAction state gstate coords@(x, y) button click =
    case (objects, button, click) of
        ([], LeftButton, DoubleClick) ->
            return $
                gstate { _getGraph = graph'
                       , _getSelMode = SelNodes [newId]
                       }
        (((k, p):_), LeftButton, SingleClick) ->
            return $
                set getSelMode (SelNodes [k]) gstate
        (((k, (Just p)):_), LeftButton, DoubleClick) ->
            case _canvasMode state of
                TGraphMode -> typeEditDialog k p state gstate
                otherwise -> nodeEditDialog k p state gstate
        otherwise -> return gstate
  where
    objects =
        filter (\p -> case p of
                          (_, Just (refCoords, _ , cf)) -> cf refCoords coords
                          otherwise -> False)
               listPayloads
    listPayloads = G.nodesWithPayload g
    g = _getGraph gstate
    (newId, graph') =
        addNode g coords (drawCircle neutralColor) (insideCircle defRadius)

addNode :: Graph
        -> Coords
        -> (Bool -> Coords -> Render ())
        -> (Coords -> Coords -> Bool)
        -> (Int, Graph)
addNode graph coords renderFunc checkFunc =
    (newId, graph')
  where
    newId = length . G.nodes $ graph
    graph' =
        G.insertNodeWithPayload newId (coords, renderFunc, checkFunc) graph

typeEditDialog :: G.NodeId -> NodePayload -> State -> GraphEditState -> IO (GraphEditState)
typeEditDialog n p@(coords, renderFunc, checkFunc) state gstate = do
    dial <- dialogNew
    cArea <- return . castToBox =<< dialogGetUpper dial
    entry <- entryNew
    colorButton <- colorButtonNewWithColor initialColor
    boxPackStart cArea entry PackNatural 1
    boxPackStart cArea colorButton PackNatural 1
--    colorButton `on` buttonPressEvent $ chooseColor
    colorSel <- colorSelectionNew
--    boxPackStart cArea colorSel PackNatural 1
    dialogAddButton dial "Apply" ResponseApply
    dialogAddButton dial "Cancel" ResponseCancel
    widgetShowAll dial
    response <- dialogRun dial
    let p' newColor = (coords, drawCircle newColor, checkFunc)
    case response of 
        ResponseApply -> do
            color <- colorButtonGetColor colorButton
            widgetDestroy dial
            return $ modify getGraph
                            (\g -> G.updateNodePayload n g (\_ -> p' color))
                            gstate
        ResponseCancel -> do
            widgetDestroy dial
            return gstate
        otherwise -> return gstate

nodeEditDialog :: G.NodeId -> NodePayload -> State -> GraphEditState -> IO (GraphEditState)
nodeEditDialog n p@(coords, renderFunc, checkFunc) state gstate = do
    dial <- dialogNew
    cArea <- return . castToBox =<< dialogGetUpper dial
    let nodeList = G.nodes $ get (getGraph . getTypeGraph) state
    store <- listStoreNew $ L.sort nodeList
    -- Create and prepare TreeView
    view <- treeViewNew
    col  <- treeViewColumnNew

    treeViewColumnSetTitle col "Node Types"
    treeViewAppendColumn view col

    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer store $
        \row -> [ cellText := show row ]
    treeViewSetModel view store
    boxPackStart cArea view PackGrow 1

    dialogAddButton dial "Apply" ResponseApply
    dialogAddButton dial "Cancel" ResponseCancel

    widgetShowAll dial
    response <- dialogRun dial
    tidRef <- newIORef 0
    view `on` cursorChanged $ do
        Just iter <- treeSelectionGetSelected =<< treeViewGetSelection view
        tid <- listStoreGetValue store $ listStoreIterToIndex iter
        writeIORef tidRef tid
    widgetDestroy dial
    case response of
        ResponseApply -> do
            -- FIXME handle safely
            tid <- readIORef tidRef
            let gstate' = modify getNodeRelation
                                 (R.update n tid)
                                 gstate
                Just (_, newRender, _) = -- FIXME
                    G.nodePayload (get (getGraph . getTypeGraph) state) tid
                updateRenderFunc g =
                    G.updateNodePayload n g (\(c, r, cf) -> (c, newRender, cf))
            return $ modify getGraph updateRenderFunc gstate'
        _ -> return gstate
    
        
    
{-
chooseColor :: EventM EButton Bool
chooseColor = do
    liftIO $ do
        dial <- colorSelectionDialogNew "Select color"
        widgetShowAll dial
        dialogRun dial
    return True
-}

currentGraph :: State -> Maybe GraphEditState
currentGraph state =
    case _canvasMode state of
        IGraphMode k -> lookup k iGraphs
        TGraphMode -> Just tGraph
        RuleMode k -> lookup k rules
  where
    iGraphs = _getInitialGraphs state
    tGraph = _getTypeGraph state
    rules = _getRules state


updateCanvas :: IORef State -> Render ()
updateCanvas stateRef = do
    state <- liftIO $ readIORef stateRef
    case _canvasMode state of
        IGraphMode k -> fetchAndRender k $ _getInitialGraphs state
        TGraphMode -> render $ _getTypeGraph state
        otherwise -> do
            renderColor $ defBorderColor
            Gtk.arc 100 40 defRadius 0 $ 2 * pi
            fill
            return ()
  where
    fetchAndRender k l =
        let graph = lookup k l
        in case graph of
            Nothing -> return ()
            Just gstate  -> render gstate
      

openFileDialog :: IO ()
openFileDialog = do
    dial <- fileChooserDialogNew
                (Just "Open") Nothing FileChooserActionOpen buttons
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
    cellLayoutSetAttributes col renderer store $
        \row -> [ cellText := show row ]

    treeViewSetModel view store
--    treeViewColumnAddAttribute col renderer "text" 0
    return view



grammarToState :: Grammar -> State
grammarToState gg =
    State (IGraphMode defGraphName) iGraphList tGraph rulesList
  where
    iGraph = GG.initialGraph gg
    iNodeRel = GM.nodeRelation iGraph
    iEdgeRel = GM.edgeRelation iGraph
    iGraphEditState =
        GraphEditState Active IdleMode (M.domain iGraph) iNodeRel iEdgeRel
    emptyRel = R.empty [] []
    tGraph =
        GraphEditState Active IdleMode (GG.typeGraph gg) emptyRel emptyRel
    iGraphList = [(defGraphName, iGraphEditState)]
    rulesList = []
--    rules  = GG.rules gg
--    ruleToGraphEditState 
--    rulesMap = foldr (\(s, r) acc -> M.insert s r acc) M.empty rules

stateToModel :: State -> IO (TreeStore TreeNode)
stateToModel state = do
    tree <- treeStoreNew [] :: IO (TreeStore TreeNode)
    treeStoreInsert tree [] 0 $ TNInitialGraph Active defGraphName
    treeStoreInsert tree [] 2 $ TNTypeGraph
--    treeStoreInsertTree tree [] 2 ruleTree
    return tree
  where
    rules = _getRules state
    tGraph = _getTypeGraph state
    

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

-- Unusual order of arguments to follow AssocList (MissingH) convension
updateAL :: [(Key, a)] -> Key -> (a -> a) -> [(Key, a)]
updateAL l k f =
    case found of
        Nothing -> l
        Just e  -> addToAL l k (f e)
  where
    found = L.lookup k l


