import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.IORef
import qualified Data.Map as M
import Data.Foldable
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Morphism
import Prelude hiding (mapM_, any)

type Coords = (Double, Double)

data LeftButtonState = EdgeCreation Int | NodeDrag Int | SelectionDrag | LeftButtonFree

data Buttons = Buttons {
    editInitialGraph :: Button,
    addRule :: Button
    }

data GUI = GUI {
    mainWindow    :: Window,
    buttons :: Buttons,
    canvas :: DrawingArea
    }

data GrammarState = GrammarState {
    getGrammar :: GG.GraphGrammar String String,
    tGraphState :: GraphState,
    graphStateMap :: M.Map Int GraphState,
    grammarCounter :: Int, -- id's from graphStates
    leftButtonState :: LeftButtonState
    }


data GraphState = GraphState {
    graph :: G.Graph String String,
    nodeTypes :: M.Map G.NodeId G.NodeId,
    edgeTypes :: M.Map G.EdgeId G.EdgeId,
    graphCounter :: Int,
    graphPos :: M.Map G.NodeId Coords
    }

radius = 20 :: Double
lineWidth = 2 :: Double
data GraphId = GraphId Int | TGraph

main = do
    initGUI
    let iGraph = GM.empty G.empty G.empty :: GM.GraphMorphism String String
        iGraphState = GraphState G.empty M.empty M.empty 0 M.empty
        tGraphState = GraphState G.empty M.empty M.empty 0 M.empty
        grammar = GG.graphGrammar G.empty iGraph []
        graphStates = M.insert 0 iGraphState $
                      M.insert 1 tGraphState $
                      M.empty
        tGraphSt = GraphState G.empty M.empty M.empty 0 M.empty
        grammarState = GrammarState grammar tGraphSt graphStates 2 LeftButtonFree
                    
    st <- newIORef grammarState
--    st <- newIORef $ GraphState iGraph M.empty 0 LeftButtonFree Nothing
    gui <- createGUI
    addMainCallBacks gui st
--    addCallBacks gui st
    --ctxt <- cairoCreateContext Nothing
    widgetShowAll $ mainWindow gui
--    widgetShow canvas
    mainGUI 

createGUI :: IO GUI
createGUI = do
    window <- windowNew
    set window [ windowTitle := "Verigraph" ]
    mainBox <- vBoxNew False 1
    containerAdd window mainBox

    iGraphButton <- buttonNewWithLabel "Edit initial graph"
    addRuleButton <- buttonNewWithLabel "Add rule"
    boxPackStart mainBox iGraphButton PackNatural 1
    boxPackStart mainBox addRuleButton PackNatural 1
    dummyCanvas <- drawingAreaNew

    let buttons = Buttons iGraphButton addRuleButton

    return $ GUI window buttons dummyCanvas 

iGraphDialog :: IORef GrammarState -> IO ()
iGraphDialog st = do
    let gId = GraphId 0
        tId = TGraph
    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog
--    contentArea <- dialogGetActionArea dialog
--    contentArea <- vBoxNew False 1
    frame <- frameNew
    frameSetLabel frame "Initial Graph"
    canvas <- drawingAreaNew
    containerAdd frame canvas

    typeFrame  <- frameNew
    typeCanvas <- drawingAreaNew
    containerAdd typeFrame typeCanvas
    frameSetLabel typeFrame "T Graph"

    canvas `on` sizeRequest $ return (Requisition 40 40)
    canvas `on` draw $ updateCanvas canvas st gId 
    canvas `on` buttonPressEvent $ mouseClick dialog st gId
    canvas `on` buttonReleaseEvent $ mouseRelease st gId
    widgetAddEvents canvas [Button1MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas st gId

    typeCanvas `on` sizeRequest $ return (Requisition 40 40)
    typeCanvas `on` draw $ updateCanvas typeCanvas st tId
    typeCanvas `on` buttonPressEvent $ mouseClick dialog st tId
    typeCanvas `on` buttonReleaseEvent $ mouseRelease st tId
    widgetAddEvents typeCanvas [Button1MotionMask]
    typeCanvas `on` motionNotifyEvent $ mouseMove typeCanvas st tId


    let cArea = castToBox contentArea
    boxPackStart cArea frame PackGrow 1
    boxPackStart cArea typeFrame PackGrow 1
    widgetShowAll dialog
    dialogRun dialog
    return ()
   

addMainCallBacks :: GUI -> IORef GrammarState -> IO ()
addMainCallBacks gui st = do
    let window   = mainWindow gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
    window `on` objectDestroy $ mainQuit
    iGraphButton `on` buttonActivated $ iGraphDialog st

    return ()

updateCanvas :: WidgetClass widget
             => widget -> IORef GrammarState -> GraphId -> Render ()
updateCanvas canvas st graphId = do
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
    drawNodes st graphId width height

drawNodes :: IORef GrammarState -> GraphId -> Double -> Double -> Render ()
drawNodes state (GraphId gId) x y = do
    st <- liftIO $ readIORef state
    setLineWidth lineWidth
    let graphState = M.lookup gId $ graphStateMap st
    case graphState of
        Just grState -> mapM_ drawNode $ graphPos grState
        otherwise -> return ()
  where
    drawNode (x, y) = do
        setSourceRGB 0 0 0
        arc x y radius 0 $ 2 * pi
        strokePreserve
        setSourceRGB 0.8 0.8 0.8
        fill

drawNodes st TGraph x y = do
    state <- liftIO $ readIORef st
    setLineWidth lineWidth
    let grState = tGraphState state
    mapM_ drawNode $ graphPos grState
  where
    drawNode (x, y) = do
        setSourceRGB 1 0 0
        arc x y radius 0 $ 2 * pi
        strokePreserve
        setSourceRGB 0.8 0.1 0.2
        fill

mouseClick :: WidgetClass widget
           => widget -> IORef GrammarState -> GraphId -> EventM EButton Bool
mouseClick widget st graphId = do
    button <- eventButton
    click  <- eventClick
    state <- liftIO $ readIORef st
    coords@(x, y) <- eventCoordinates
    let newState = case (button, click) of
            (LeftButton, DoubleClick) -> leftDoubleClick state graphId coords
            (LeftButton, SingleClick) -> leftSingleClick state graphId coords
            otherwise                 -> state
    liftIO $ writeIORef st newState
    liftIO $ widgetQueueDraw widget
    return True

mouseRelease :: IORef GrammarState -> GraphId -> EventM EButton Bool
mouseRelease st gId = do
    liftIO $ modifyIORef st cancelDrag
    return True
  where
    cancelDrag gramState =
        gramState { leftButtonState = LeftButtonFree }

mouseMove :: WidgetClass widget
          => widget -> IORef GrammarState -> GraphId -> EventM EMotion Bool
mouseMove widget st graphId = do
    state <- liftIO $ readIORef st
    coords <- eventCoordinates
    let graphState = case graphId of
                        GraphId gId -> M.lookup gId $ graphStateMap state
                        TGraph -> Just $ tGraphState state
        lButSt = leftButtonState state
    case graphState of
        Nothing -> return True
        Just grState -> do
            processLeftButton lButSt grState coords
            return True
  where
    processLeftButton (NodeDrag nId) grState coords =
        liftIO $ do modifyIORef st $ updateCoords grState nId coords
                    widgetQueueDraw widget
    processLeftButton _ _ _ = return ()
    newGraphState grState nId newCoords =
        let grPos = graphPos grState in
            grState { graphPos = M.insert nId newCoords grPos }
    updateCoords grState nId newCoords st =
        case graphId of
        GraphId gId ->
            let grStates = graphStateMap st in
                st { graphStateMap =
                        M.insert gId
                                 (newGraphState grState nId newCoords) grStates
                   }
        TGraph -> st { tGraphState = newGraphState grState nId newCoords }

leftDoubleClick :: GrammarState -> GraphId -> Coords -> GrammarState
leftDoubleClick state graphId coords =
    case graphState graphId of
        Nothing -> state
        Just grState ->
            let posMap = graphPos grState
            in if isOverAnyNode coords posMap
                then state
                else newNode graphId coords state
  where
    graphState (GraphId gId) = M.lookup gId $ graphStateMap state
    graphState TGraph = Just $ tGraphState state


leftSingleClick :: GrammarState -> GraphId -> Coords -> GrammarState
leftSingleClick state graphId coords =
    case graphState graphId of
        Nothing -> state
        Just grState -> do
            let posMap = graphPos grState
            case nodeId posMap of
                Just k -> nodeDrag k state
                otherwise -> selDrag state
  where
    graphState (GraphId gId) = M.lookup gId $ graphStateMap state
    graphState TGraph = Just $ tGraphState state
    nodeId posMap = checkNodeClick coords posMap
    nodeDrag newId gramState =
        gramState { leftButtonState = NodeDrag newId }
    selDrag gramState =
        gramState { leftButtonState = SelectionDrag }

checkNodeClick :: Coords -> M.Map G.NodeId Coords -> Maybe G.NodeId
checkNodeClick coords posMap =
    case found of
    (x:xs)    -> Just $ fst x
    otherwise -> Nothing     
  where
    found = filter insideNode $ M.toList posMap 
    insideNode (_, nodeCoords) =
        distance coords nodeCoords <= radius
    
isOverAnyNode :: Coords -> M.Map G.NodeId Coords -> Bool
isOverAnyNode coords posMap =
    case checkNodeClick coords posMap of
    Just k    -> True
    otherwise -> False

distance :: Coords -> Coords -> Double
distance (x0, y0) (x1, y1) =
    sqrt $ (square (x1 - x0)) + (square (y1 - y0))
  where
    square x = x * x

newNode :: GraphId -> Coords -> GrammarState -> GrammarState
newNode graphId coords st =
    newGramState graphId
  where
    grStates = graphStateMap st
    graphState (GraphId gId) = M.lookup gId grStates
    graphState TGraph = Just $ tGraphState st
    (Just grState) = graphState graphId -- FIX unsafe
    gr = graph $ grState
    gr' = G.insertNode nId gr
    (nId, grPos) = (graphCounter grState, graphPos grState)
    grPos' = M.insert nId coords grPos
    grState' =
        GraphState gr' (nodeTypes grState) (edgeTypes grState) (nId + 1) grPos'
    newGramState (GraphId gId) =
        st { graphStateMap  = M.insert gId grState' grStates }
    newGramState TGraph =
        st { tGraphState = grState' }
    
