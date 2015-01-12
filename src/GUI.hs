import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import Data.IORef
import qualified Data.Map as M
import Data.Foldable
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Morphism
import Prelude hiding (mapM_, any)

type Coords = (Double, Double)

data LeftButtonState = NodeDrag | SelectionDrag | LeftButtonFree

data Buttons = Buttons {
    editInitialGraph :: Button,
    addRule :: Button
    }

data GUI = GUI {
    mainWindow    :: Window,
    buttons :: Buttons,
    canvas :: DrawingArea
    }

data GraphState = GraphState {
    graph    :: GM.TypedGraph String String,
    typeGraph       :: G.Graph String String,
    graphPos :: M.Map G.NodeId Coords,
    counter         :: Int,
    leftButtonState :: LeftButtonState,
    currentNodeId   :: Maybe G.NodeId
    }

radius = 20 :: Double

main = do
    initGUI
    let iSimpleGraph = G.empty :: G.Graph String String
        tGraph = G.empty :: G.Graph String String
        iGraph = GM.empty iSimpleGraph tGraph
    st <- newIORef $ GraphState iGraph tGraph M.empty 0 LeftButtonFree Nothing
    gui <- createGUI
    addMainCallBacks gui st
--    addCallBacks gui st
    --ctxt <- cairoCreateContext Nothing
    showGUI gui
--    widgetShowAll window
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

iGraphDialog :: IORef GraphState ->  IO ()
iGraphDialog st = do
    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog
--    contentArea <- dialogGetActionArea dialog
--    contentArea <- vBoxNew False 1
    frame <- frameNew
    frameSetLabel frame "Initial Graph"
    canvas <- drawingAreaNew
    containerAdd frame canvas

    typeFrame    <- frameNew
    frameSetLabel typeFrame "T Graph"

    canvas `on` sizeRequest $ return (Requisition 40 40)
    canvas `on` draw $ updateCanvas canvas st
    canvas `on` buttonPressEvent $ mouseClick dialog st
    canvas `on` buttonReleaseEvent $ mouseRelease st
    widgetAddEvents canvas [Button1MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove dialog st

    let cArea = castToBox contentArea
    boxPackStart cArea frame PackGrow 1
--    boxPackStart cArea typeButton PackGrow 1
    widgetShowAll dialog
    putStrLn "iGraphDialog"
    dialogRun dialog
    return ()
   

addMainCallBacks :: GUI -> IORef GraphState -> IO ()
addMainCallBacks gui st = do
    let window   = mainWindow gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
    window `on` objectDestroy $ mainQuit
    iGraphButton `on` buttonActivated $ iGraphDialog st

    return ()


showGUI gui = do
    let mainWin   = mainWindow gui
        iGrCanvas = canvas gui
    widgetShowAll mainWin
--    widgetShow    iGrCanvas

updateCanvas :: WidgetClass widget
             => widget
             -> IORef GraphState
             -> Render ()
updateCanvas canvas st = do
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
    drawNodes st width height

drawNodes :: IORef GraphState -> Double -> Double -> Render ()
drawNodes state x y = do
    st <- liftIO $ readIORef state
    setLineWidth 2
    let posMap = graphPos st
    mapM_ drawNode posMap
  where
    drawNode (x, y) = do
        setSourceRGB 0 0 0
        arc x y radius 0 $ 2 * pi
        strokePreserve
        setSourceRGB 0.8 0.8 0.8
        fill

mouseClick :: WidgetClass widget => widget -> IORef GraphState -> EventM EButton Bool
mouseClick widget st = do
    button <- eventButton
    click  <- eventClick
    coords@(x, y) <- eventCoordinates
    case (button, click) of
        (LeftButton, DoubleClick) -> liftIO $ leftDoubleClick widget st coords
        (LeftButton, SingleClick) -> liftIO $ leftSingleClick widget st coords
        otherwise                 -> liftIO $ putStrLn "Unknown button"
    return True

mouseRelease :: IORef GraphState -> EventM EButton Bool
mouseRelease st = do
    liftIO $ modifyIORef st cancelDrag
    return True
  where
    cancelDrag (GraphState iGr tGr iGrPos c _ curNId) =
        GraphState iGr tGr iGrPos c LeftButtonFree curNId

mouseMove :: WidgetClass widget => widget -> IORef GraphState -> EventM EMotion Bool
mouseMove widget st = do
    state <- liftIO $ readIORef st
    processLeftButton $ leftButtonState state
    return True
  where
    processLeftButton NodeDrag = do
        coords <- eventCoordinates
        liftIO $ do modifyIORef st $ updateCoords coords
                    widgetQueueDraw widget
    processLeftButton _ = return ()
    updateCoords newCoords (GraphState iGr tGr iGrPos c lSt (Just curNId)) =
        GraphState iGr tGr (M.insert curNId newCoords iGrPos) c lSt (Just curNId)

    

leftDoubleClick :: WidgetClass widget => widget -> IORef GraphState -> Coords -> IO ()
leftDoubleClick widget st coords = do
    state <- liftIO $ readIORef st
    let posMap = graphPos state
    if isOverAnyNode coords posMap
    then putStrLn $ "clicked over node" ++ (show coords)
    else do modifyIORef st (newNode coords)
            widgetQueueDraw widget


leftSingleClick :: WidgetClass widget => widget -> IORef GraphState -> Coords -> IO ()
leftSingleClick widget st coords = do
    state <- liftIO $ readIORef st
    let posMap = graphPos state
    case nodeId posMap of
        newId@(Just k) -> modifyIORef st $ nodeDrag newId
        otherwise      -> modifyIORef st $ selDrag
  where
    nodeId posMap = checkNodeClick coords posMap
    nodeDrag newId (GraphState iGr tGr iGrPos c _ _) =
        GraphState iGr tGr iGrPos c NodeDrag newId
    selDrag (GraphState iGr tGr iGrPos c _ curNId) =
        GraphState iGr tGr iGrPos c SelectionDrag curNId

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

newNode :: Coords -> GraphState -> GraphState
newNode coords st =
    GraphState gr' tGr pos (id + 1) lState (Just id)
  where
    id = counter st
    gr = graph st
    (dom, cod, nR, eR) =
        (domain gr, codomain gr, GM.nodeRelation gr, GM.edgeRelation gr)
    gr' = GM.graphMorphism (G.insertNode id dom) cod nR eR
    pos = M.insert id coords $ graphPos st
    lState = leftButtonState st
    tGr = typeGraph st
    
