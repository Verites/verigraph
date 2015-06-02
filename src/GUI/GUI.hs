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

onEdge :: Coords -> Coords -> Coords -> Coords -> Bool
onEdge src@(x, y) tgt@(x', y') coords bendVect =
    norm coords eCenter <= radius ||
    norm coords ctrlP1 <= radius ||
    norm coords ctrlP2 <= radius
  where 
    (dx, dy) = directionVect src tgt
    dist = norm src tgt
    (ctrlP1, ctrlP2) = ctrlPoints src tgt bendVect
    eCenter = edgeCenter src tgt ctrlP1 ctrlP2
    radius = 2 * defRadius -- defRadius is arbitrary, meant as a test

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
    window `on` keyPressEvent $ tryEvent $ keyPress canvas stateRef
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
                    let n@(G.NodeId nid) = get freeNodeId gstate in
                    set refCoords coords $
                    set freeNodeId (G.NodeId (nid + 1)) $
                    gstate { _getGraph = graph'
                           , _selObjects = [Node n p']
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
                        modify getGraph (addEdge src k) $ 
                        modify freeEdgeId (\(G.EdgeId i) -> G.EdgeId (i + 1)) gstate
                    otherwise -> set mouseMode (EdgeCreation k) $
                                 gstate
            otherwise -> gstate
        otherwise -> return gstate
  where
    objects = nodeObjects ++ edgeObjects
    edgeObjects =
        -- the Edge constructor transforms EdgeId into an Obj
        map (\(k, Just p) -> Edge k p) $
        filter (\(_, p) ->
                    let res = do
                        (EdgePayload _ src tgt bendVect cf) <- p
                        (NodePayload _ srcC _ _) <- G.nodePayload g src
                        (NodePayload _ tgtC _ _) <- G.nodePayload g tgt
                        return $ cf srcC tgtC coords bendVect
                    in case res of
                        Just True -> True
                        otherwise -> False)
               (G.edgesWithPayload g)
    nodeObjects =
        -- the Node constructor transforms NodeId into an Obj
        -- No payload-less nodes pass filtering
        map (\(k, Just p) -> Node k p) $ 
        filter (\(_, p) -> case p of
                          Just p -> (get nodeCheck p) (get nodeCoords p) coords
                          otherwise -> False)
               (G.nodesWithPayload g)
    g = _getGraph gstate
    (p', graph') =
        addNode gstate coords (drawNode neutralColor)
                         (insideCircle defRadius)
    addEdge src tgt gr =
        let newId = get freeEdgeId gstate
            bendVect = (0, -100)
        in G.insertEdgeWithPayload
               newId src tgt (EdgePayload newId src tgt bendVect onEdge) gr

addNode :: GraphEditState
        -> Coords
        -> (GramState -> GraphEditState -> G.NodeId -> Render ())
        -> (Coords -> Coords -> Bool)
        -> (NodePayload, Graph)
addNode gstate coords renderFunc checkFunc =
    (p, graph')
  where
    newId = get freeNodeId gstate
    p = NodePayload newId coords renderFunc checkFunc
    graph = get getGraph gstate
    graph' =
        G.insertNodeWithPayload newId p graph

typeEditDialog :: G.NodeId -> NodePayload -> GramState -> GraphEditState -> IO (GraphEditState)
typeEditDialog n p@(NodePayload nid coords renderFunc checkFunc) state gstate = do
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
    let p' newColor = NodePayload nid coords (drawNode newColor) checkFunc
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
nodeEditDialog n p@(NodePayload _ coords renderFunc checkFunc) state gstate = do
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
                    G.updateNodePayload n g (set nodeRender nodeRenderType)
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
            G.updateNodePayload n g (modify nodeCoords (\(x, y) -> (x + dx, y + dy)))
        updateBendVect e g =
            G.updateEdgePayload
                e g (modify bendVect (\(bx, by) -> (bx + dx, by + dy)))
        selObjs = get selObjects gstate
        updateAllNodes g =
            foldr (\n acc -> case n of
                                Node n _ -> updateCoords acc n
                                _ -> acc)
                  g
                  selObjs
        gstate' = set refCoords coords gstate
        gstate'' =
            case selObjs of
                [Edge e _] -> modify getGraph (updateBendVect e) gstate'
                _ -> modify getGraph updateAllNodes gstate'
    liftIO $ writeIORef stateRef $ setCurGraphState gstate'' state
    liftIO $ widgetQueueDraw canvas
    return True

keyPress :: WidgetClass widget
           => widget -> IORef GramState -> EventM EKey ()
keyPress canvas stateRef = do
    key <- eventKeyName
    state <- liftIO $ readIORef stateRef
    let mgstate = currentGraphState state
    case (mgstate, key) of
        (Just gstate, "Delete") -> do
            let sel = L.partition isEdge $ get selObjects gstate
                gstate' = modify getGraph (deleteObjects sel) gstate
            liftIO $ writeIORef stateRef $ setCurGraphState gstate' state
        _ -> return ()
    liftIO $ widgetQueueDraw canvas
  where
    isEdge (Edge _ _) = True
    isEdge _ = False
    deleteEdges edges gr = foldr G.removeEdge gr edges
    deleteNodes nodes gr = foldr G.removeNode gr nodes
    deleteObjects sel =
        let selEdges = map (\(Edge e _) -> e) $ fst sel
            selNodes = map (\(Node n _) -> n) $ snd sel
        in deleteNodes selNodes . deleteEdges selEdges
        
        

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


