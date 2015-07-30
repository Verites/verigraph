{-# LANGUAGE TemplateHaskell #-} -- fclabels

module GUI.GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import qualified Graph.Graph as G
import GUI.Editing
import GUI.Render

import qualified Abstract.Relation as R
import Abstract.Valid (valid)
import Control.Applicative
import Control.Category -- for fclabels, including (.) and id
import Control.Monad (filterM)
import Data.AdditiveGroup ((^+^), (^-^))
import Data.AffineSpace (distance)
import Data.Label -- fclabels
import qualified Data.List as L
import Data.List.Utils
import Data.Maybe (mapMaybe)
import Data.IORef
import qualified Data.Tree as T ( Tree( Node ))
import Data.VectorSpace ((^*), lerp, magnitude, normalized)
import Debug.Trace
import Graphics.UI.Gtk hiding (get, set) -- conflict with fclabels
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Gtk
import Prelude hiding (mapM_, any, (.), id)



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
    distance circleCoords coords <= radius

onEdge :: Coords -> Coords -> Coords -> Coords -> Coords -> Maybe CtrlPoint
onEdge srcC@(x, y) tgtC@(x', y') coords cp1 cp2
--    distance coords eCenter <= radius ||
    | distance coords cp1 <= radius = Just 0
    | distance coords cp2 <= radius = Just 1
    | otherwise = Nothing
  where 
--    eCenter = edgeCenter src tgt ctrlP1 ctrlP2
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
                -- select object
                return $
                    set refCoords coords $
                    (case (multiSel, x `L.elem` get selObjects gstate) of
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
            ((Node n p:_), SingleClick) -> do
                case _mouseMode gstate of
                    EdgeCreation src srcP ->
                        modify getGraph (addEdge src srcP n p) $ 
                        modify freeEdgeId (\(G.EdgeId i) -> G.EdgeId (i + 1)) gstate
                    otherwise -> set mouseMode (EdgeCreation n p) $
                                 gstate
            otherwise -> gstate
        otherwise -> return gstate
  where
    objects = nodeObjects ++ edgeObjects
    edgeObjects =
        -- the Edge constructor transforms EdgeId into an Obj
--        map (\(k, Just p, cf) -> Edge k p [cf]) $
        mapMaybe (\(k, p) ->
                    let res = do
                        (EdgePayload _ src tgt ctrlP1 ctrlP2 cf) <- p
                        (NodePayload _ srcC _ _) <- G.nodePayload g src
                        (NodePayload _ tgtC _ _) <- G.nodePayload g tgt
                        cf srcC tgtC coords ctrlP1 ctrlP2
                    in case res of
                        Just cp -> p >>= (\p' -> Just $ Edge k p' [cp])
                        otherwise -> Nothing)
               $ G.edgesWithPayload g
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
        addNode gstate coords (drawCircle neutralColor)
                         (insideCircle defRadius)
    addEdge src srcP tgt tgtP gr =
        let newId = get freeEdgeId gstate
            -- FIXME correct
            srcC = get nodeCoords srcP
            tgtC = get nodeCoords tgtP
            diffV = tgtC ^-^ srcC
--            len = magnitude $ tgtC ^-^ srcC
            ctrlP1 = srcC ^+^ diffV ^* 0.25
            ctrlP2 = srcC ^+^ diffV ^* 0.75
        in G.insertEdgeWithPayload
               newId src tgt (EdgePayload newId src tgt ctrlP1 ctrlP2 onEdge) gr

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
    -- box to group all radio buttons according to available shapes
    nodeShapeBox <- vBoxNew False 2
    circleButton <- radioButtonNewWithLabel "Circle" 
    squareButton <- radioButtonNewWithLabelFromWidget circleButton "Square" 
    boxPackStart nodeShapeBox circleButton PackGrow 2
    boxPackStart nodeShapeBox squareButton PackGrow 2
    boxPackStart cArea nodeShapeBox PackGrow 2
    
    dialogAddButton dial "Apply" ResponseApply
    dialogAddButton dial "Cancel" ResponseCancel
    widgetShowAll dial
    response <- dialogRun dial
    case response of 
        ResponseApply -> do
            color <- colorButtonGetColor colorButton
            shapeButtons <- radioButtonGetGroup circleButton
            shapeButtons' <- filterM toggleButtonGetActive shapeButtons
            shapeStr <- buttonGetLabel $ head shapeButtons'
            let shapeFunc = case shapeStr of
                             "Square" -> drawSquare
                             "Circle" -> drawCircle
                             _ -> drawCircle --FIXME
                p' newColor = set nodeRender (shapeFunc newColor) p
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
        refC = _refCoords gstate
        deltaC = coords ^-^ refC
        updateCoords n g =
            G.updateNodePayload n g (modify nodeCoords (\coords -> coords ^+^ deltaC))
        -- updates all selected control points from g
        updateCtrlP e ctrlPts g =
            foldr (\ctrlP g ->
                    case ctrlP of
                    0 -> G.updateEdgePayload
                            e g (modify ctrlP1 (\baseC -> baseC ^+^ deltaC))
                    1 -> G.updateEdgePayload
                            e g (modify ctrlP2 (\baseC -> baseC ^+^ deltaC))
                    _ -> g)

                  g
                  ctrlPts
        selObjs = get selObjects gstate
        updateAllObjs g =
            foldr (\n acc -> case n of
                                Node n _ -> updateCoords n acc
                                Edge e _ ctrlPts -> updateCtrlP e ctrlPts acc)
                  g
                  selObjs
        gstate' = set refCoords coords gstate
        gstate'' =
            case selObjs of
--                [Edge e _ ctrlPLst] -> modify getGraph (updateCtrlP e ctrlPLst) gstate'
                (_:_) -> modify getGraph updateAllObjs gstate'
                _ -> gstate'
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
    isEdge (Edge _ _ _) = True
    isEdge _ = False
    deleteEdges edges gr = foldr G.removeEdge gr edges
    deleteNodes nodes gr = foldr G.removeNode gr nodes
    deleteObjects sel =
        let selEdges = map (\(Edge e _ _) -> e) $ fst sel
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


