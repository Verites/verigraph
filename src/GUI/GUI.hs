{-# LANGUAGE TemplateHaskell #-} -- fclabels

module GUI.GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import Control.Monad.Trans.Class (lift)
import qualified Graph.Graph as G
import qualified Graph.GraphMorphism as GM
import qualified Graph.GraphGrammar as GG
import qualified Graph.GraphRule as GR
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
import qualified Abstract.Morphism as M
import Prelude hiding (mapM_, any, (.), id)
import Control.Category -- for fclabels, including (.) and id
import qualified Abstract.Relation as R
import Abstract.Valid (valid)

import GUI.Render
import GUI.Editing


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

$(mkLabels [''GUI, ''Buttons])


-- FIXME temporary magic constants
neutralColor = Color 13363 25956 42148 -- gainsboro
initialColor = Color 13363 25956 42148

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
    widgetAddEvents canvas [Button1MotionMask]
--    canvas `on` motionNotifyEvent $ 
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
    let Just gstate = currentGraphState state -- FIXME unsafe pattern matching
    gstate' <- liftIO $ chooseMouseAction state gstate coords button click
    liftIO $ writeIORef stateRef $ setCurGraphState gstate' state
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
                       , _getSelMode = SelObjects [Node newId]
                       }
        (((k, p):_), LeftButton, SingleClick) ->
            return $
                set getSelMode (SelObjects [Node k]) $
                set refCoords coords gstate
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
        addNode g coords (drawCircle neutralColor)
                         (insideCircle defRadius)

addNode :: Graph
        -> Coords
        -> (State -> GraphEditState -> G.NodeId -> Render ())
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
    tidRef <- newIORef 0
    view `on` cursorChanged $ do
        Just iter <- treeSelectionGetSelected =<< treeViewGetSelection view
        tid <- listStoreGetValue store $ listStoreIterToIndex iter
        writeIORef tidRef tid

    response <- dialogRun dial
    widgetDestroy dial
    case response of
        ResponseApply -> do
            -- FIXME handle safely
            tid <- readIORef tidRef
            let gstate' = modify getNodeRelation
                                 (R.update n tid)
                                 gstate
                updateRenderFunc g =
                    G.updateNodePayload n g (\(c, _, cf) -> (c, nodeRenderType, cf))
            return $ modify getGraph updateRenderFunc gstate'
        _ -> do
            return gstate

{-
mouseMove :: WidgetClass widget
          => widget -> IORef State -> EventM EButton Bool
mouseMove canvas stateRef = do
    coords <- eventCoordinates
    state <- liftIO $ readIORef stateRef
    let gstate = currentGraphState state
        refCoords = get refCoords gstate
-}


updateCanvas :: IORef State -> Render ()
updateCanvas stateRef = do
    state <- liftIO $ readIORef stateRef
    render state


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


-- Unusual order of arguments to follow AssocList (MissingH) convension
updateAL :: [(Key, a)] -> Key -> (a -> a) -> [(Key, a)]
updateAL l k f =
    case found of
        Nothing -> l
        Just e  -> addToAL l k (f e)
  where
    found = L.lookup k l

