{-# LANGUAGE TemplateHaskell #-} -- fclabels

module GUI.GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import qualified Graph.Graph as G
import GUI.Render
import GUI.Editing

import Data.Label -- fclabels
import Data.List.Utils
import Data.IORef
import qualified Data.Tree as T ( Tree( Node ))
import qualified Data.List as L
import Debug.Trace
import Control.Applicative
import Graphics.UI.Gtk hiding (get, set) -- conflict with fclabels
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Gtk
import Prelude hiding (mapM_, any, (.), id)
import Control.Category -- for fclabels, including (.) and id
import qualified Abstract.Relation as R
import Abstract.Valid (valid)



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

-- functions to check for clicked objects
insideCircle :: Double -> Coords -> Coords -> Bool
insideCircle radius circleCoords coords =
    norm circleCoords coords <= radius

onEdge :: Coords -> Coords -> Coords -> Double -> Bool
onEdge src@(x, y) tgt@(x', y') coords bendFactor =
    trace ("src: " ++ show src ++
           "\ttgt: " ++ show tgt ++
           "\tctrlP1: " ++ show ctrlP1 ++
           "\tctrlP2: " ++ show ctrlP2 ++
           "\teCenter: " ++ show eCenter ++
           "\tcoords: " ++ show coords) $
    norm coords eCenter <= defRadius -- defRadius is arbitrary, meant as a test
  where 
    (dx, dy) = directionVect src tgt
    dist = norm src tgt
    (ctrlP1, ctrlP2) = ctrlPoints src tgt bendFactor
    eCenter = edgeCenter src tgt ctrlP1 ctrlP2

runGUI :: IO ()
runGUI = do
    let state = grammarToState testGrammar
    stateRef <- newIORef state
    gui <- createGUI state
    showGUI gui
    addMainCallbacks gui stateRef
    return ()

showGUI = widgetShowAll . _mainWindow

createGUI :: GramState -> IO GUI
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

addMainCallbacks :: GUI -> IORef GramState -> IO ()
addMainCallbacks gui stateRef = do
    let window   = _mainWindow gui
        view = _treeView gui
        store = _treeStore gui
        canvas = _getCanvas gui
    window `on` objectDestroy $ mainQuit
    canvas `on` buttonPressEvent $ tryEvent $ mouseClick canvas stateRef
    dwin <- widgetGetDrawWindow canvas
    canvas `on` exposeEvent $ do liftIO $ renderWithDrawable dwin (updateCanvas stateRef)
                                 return True
    widgetAddEvents canvas [Button1MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas stateRef
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
           => widget -> IORef GramState -> EventM EButton ()
mouseClick canvas stateRef = do
    coords <- eventCoordinates
    button <- eventButton
    click <- eventClick
    state <- liftIO $ readIORef stateRef
    modifiers <- eventModifier
    let mgstate = currentGraphState state -- FIXME unsafe pattern matching
        multiSel = Control `L.elem` modifiers
    case mgstate of
        Just gstate -> do
            gstate' <- liftIO $
                chooseMouseAction state gstate coords button click multiSel
            liftIO $ writeIORef stateRef $ setCurGraphState gstate' state
            liftIO $ widgetQueueDraw canvas
        _ -> return ()

chooseMouseAction :: GramState
                  -> GraphEditState
                  -> Coords
                  -> MouseButton
                  -> Click
                  -> Bool -- ^ multiSel: control is pressed
                  -> IO GraphEditState
chooseMouseAction state gstate coords@(x, y) button click multiSel =
    case button of
        LeftButton ->
            fmap (set mouseMode SelMode) $
            case (objects, click) of
            ([], DoubleClick) ->
                -- create node
                return $
                    set refCoords coords $
                    gstate { _getGraph = graph'
                           , _selObjects = [Node newId p']
                           }
            ([], SingleClick) ->
                -- unselect nodes
                return $ set selObjects [] gstate
            ((x:_), SingleClick) -> 
                -- select node
                return $
                    set refCoords coords $
                    (case (multiSel, x `L.elem` _selObjects gstate) of
                          (False, False) -> set selObjects [x]
                          (True, False) -> modify selObjects (x:)
                          (True, True) -> modify selObjects (L.delete x)
                          _ -> id) $
                    gstate
            ((n@(Node k p):_), DoubleClick) ->
                -- open editing dialog
                case _canvasMode state of
                    TGraphMode -> typeEditDialog k p state gstate
                    otherwise -> nodeEditDialog k p state gstate
            otherwise -> return gstate
        RightButton ->
            return $
            case (objects, click) of
            ((Node k p:_), SingleClick) -> do
                case _mouseMode gstate of
                    EdgeCreation src ->
                        modify getGraph (addEdge src k) gstate
                    otherwise -> set mouseMode (EdgeCreation k) gstate
            otherwise -> gstate
        otherwise -> return gstate
  where
    objects = nodeObjects ++ edgeObjects
    edgeObjects =
        -- the Edge constructor transforms EdgeId into an Obj
        map (\(k, Just p) -> Edge k p) $
        filter (\(_, p) ->
                    let res = do
                        (src, tgt, bendFactor, cf) <- p
                        (srcC, _, _) <- G.nodePayload g src
                        (tgtC, _, _) <- G.nodePayload g tgt
                        return $ cf srcC tgtC coords bendFactor
                    in case res of
                        Just True -> True
                        otherwise -> False)
               (G.edgesWithPayload g)
    nodeObjects =
        -- the Node constructor transforms NodeId into an Obj
        -- No payload-less nodes pass filtering
        map (\(k, Just p) -> Node k p) $ 
        filter (\(_, p) -> case p of
                          Just (refCoords, _ , cf) -> cf refCoords coords
                          otherwise -> False)
               (G.nodesWithPayload g)
    g = _getGraph gstate
    (newId, p', graph') =
        addNode g coords (drawNode neutralColor)
                         (insideCircle defRadius)
    addEdge src tgt gr =
        let newId = G.EdgeId . length . G.edges $ gr
            bendFactor = 2
        in G.insertEdgeWithPayload
               newId src tgt (src, tgt, bendFactor, onEdge) gr

addNode :: Graph
        -> Coords
        -> (GramState -> GraphEditState -> G.NodeId -> Render ())
        -> (Coords -> Coords -> Bool)
        -> (G.NodeId, NodePayload, Graph)
addNode graph coords renderFunc checkFunc =
    (newId, p, graph')
  where
    newId = G.NodeId . length . G.nodes $ graph
    p = (coords, renderFunc, checkFunc)
    graph' =
        G.insertNodeWithPayload newId p graph

typeEditDialog :: G.NodeId -> NodePayload -> GramState -> GraphEditState -> IO (GraphEditState)
typeEditDialog n p@(coords, renderFunc, checkFunc) state gstate = do
    dial <- dialogNew
    cArea <- return . castToBox =<< dialogGetUpper dial
    entry <- entryNew
    colorButton <- colorButtonNewWithColor neutralColor
    boxPackStart cArea entry PackNatural 1
    boxPackStart cArea colorButton PackNatural 1
--    colorButton `on` buttonPressEvent $ chooseColor
    colorSel <- colorSelectionNew
--    boxPackStart cArea colorSel PackNatural 1
    dialogAddButton dial "Apply" ResponseApply
    dialogAddButton dial "Cancel" ResponseCancel
    widgetShowAll dial
    response <- dialogRun dial
    let p' newColor = (coords, drawNode newColor, checkFunc)
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

nodeEditDialog :: G.NodeId -> NodePayload -> GramState -> GraphEditState -> IO (GraphEditState)
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
    tidRef <- newIORef $ G.NodeId 0
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

mouseMove :: WidgetClass widget
          => widget -> IORef GramState -> EventM EMotion Bool
mouseMove canvas stateRef = do
    coords@(x, y) <- eventCoordinates
    state <- liftIO $ readIORef stateRef
    let Just gstate = currentGraphState state -- FIXME
        (refX, refY) = _refCoords gstate
        (dx, dy) = (x - refX, y - refY)
        updateCoords g n =
            G.updateNodePayload n g (\((x, y), rf, cf) ->
                                            ((x + dx, y + dy), rf, cf))
        selObjs = get selObjects gstate -- FIXME
        updateAllNodes g =
            foldr (\n acc -> case n of
                                Node n _ -> updateCoords acc n
                                _ -> acc)
                  g
                  selObjs
        gstate' = set refCoords coords gstate
        gstate'' = modify getGraph updateAllNodes gstate'
    liftIO $ writeIORef stateRef $ setCurGraphState gstate'' state
    liftIO $ widgetQueueDraw canvas
    return True


updateCanvas :: IORef GramState -> Render ()
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

createModel :: GramState -> IO (TreeStore TreeNode)
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


