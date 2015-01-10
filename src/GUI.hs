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

data GUI = GUI {
    mainWindow    :: Window,
    initialCanvas :: DrawingArea
    }

data GrammarState = GrammarState {
    initialGraph    :: GM.TypedGraph String String,
    typeGraph       :: G.Graph String String,
    initialGraphPos :: M.Map G.NodeId Coords,
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
    st <- newIORef $ GrammarState iGraph tGraph M.empty 0 LeftButtonFree Nothing
    gui <- createGUI
    addCallBacks gui st
    --ctxt <- cairoCreateContext Nothing
    showGUI gui
--    widgetShowAll window
--    widgetShow canvas
    mainGUI 

createGUI :: IO GUI
createGUI = do
    window <- windowNew
    set window [ windowTitle := "Verigraph" ]
    canvas <- drawingAreaNew
    containerAdd window canvas
    return $ GUI window canvas

addCallBacks :: GUI -> IORef GrammarState -> IO ()
addCallBacks gui st = do
    let mainWin   = mainWindow gui
        iGrCanvas = initialCanvas gui
    mainWin `on` objectDestroy $ mainQuit
    iGrCanvas `on` sizeRequest $ return (Requisition 40 40)
    iGrCanvas `on` draw $ updateCanvas iGrCanvas st
    iGrCanvas `on` buttonPressEvent $ mouseClick gui st
    iGrCanvas `on` buttonReleaseEvent $ mouseRelease st
    widgetAddEvents iGrCanvas [Button1MotionMask]
    iGrCanvas `on` motionNotifyEvent $ mouseMove gui st
    return ()

showGUI gui = do
    let mainWin   = mainWindow gui
        iGrCanvas = initialCanvas gui
    widgetShowAll mainWin
    widgetShow    iGrCanvas

updateCanvas :: WidgetClass widget
             => widget
             -> IORef GrammarState
             -> Render ()
updateCanvas canvas st = do
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
    drawNodes st width height

drawNodes :: IORef GrammarState -> Double -> Double -> Render ()
drawNodes state x y = do
    st <- liftIO $ readIORef state
    setLineWidth 2
    let posMap = initialGraphPos st
    mapM_ drawNode posMap
  where
    drawNode (x, y) = do
        setSourceRGB 0 0 0
        arc x y radius 0 $ 2 * pi
        strokePreserve
        setSourceRGB 0.8 0.8 0.8
        fill

mouseClick :: GUI -> IORef GrammarState -> EventM EButton Bool
mouseClick gui st = do
    button <- eventButton
    click  <- eventClick
    coords@(x, y) <- eventCoordinates
    case (button, click) of
        (LeftButton, DoubleClick) -> liftIO $ leftDoubleClick gui st coords
        (LeftButton, SingleClick) -> liftIO $ leftSingleClick gui st coords
        otherwise                 -> liftIO $ putStrLn "Unknown button"
    return True

mouseRelease :: IORef GrammarState -> EventM EButton Bool
mouseRelease st = do
    liftIO $ modifyIORef st cancelDrag
    return True
  where
    cancelDrag (GrammarState iGr tGr iGrPos c _ curNId) =
        GrammarState iGr tGr iGrPos c LeftButtonFree curNId

mouseMove :: GUI -> IORef GrammarState -> EventM EMotion Bool
mouseMove gui st = do
    state <- liftIO $ readIORef st
    processLeftButton $ leftButtonState state
    return True
  where
    processLeftButton NodeDrag = do
        coords <- eventCoordinates
        liftIO $ do modifyIORef st $ updateCoords coords
                    widgetQueueDraw $ mainWindow gui
    processLeftButton _ = return ()
    updateCoords newCoords (GrammarState iGr tGr iGrPos c lSt (Just curNId)) =
        GrammarState iGr tGr (M.insert curNId newCoords iGrPos) c lSt (Just curNId)

    

leftDoubleClick :: GUI -> IORef GrammarState -> Coords -> IO ()
leftDoubleClick gui st coords = do
    state <- liftIO $ readIORef st
    let posMap = initialGraphPos state
    if isOverAnyNode coords posMap
    then putStrLn $ "clicked over node" ++ (show coords)
    else do modifyIORef st (newNode coords)
            widgetQueueDraw $ mainWindow gui


leftSingleClick :: GUI -> IORef GrammarState -> Coords -> IO ()
leftSingleClick gui st coords = do
    state <- liftIO $ readIORef st
    let posMap = initialGraphPos state
    case nodeId posMap of
        newId@(Just k) -> modifyIORef st $ nodeDrag newId
        otherwise      -> modifyIORef st $ selDrag
  where
    nodeId posMap = checkNodeClick coords posMap
    nodeDrag newId (GrammarState iGr tGr iGrPos c _ _) =
        GrammarState iGr tGr iGrPos c NodeDrag newId
    selDrag (GrammarState iGr tGr iGrPos c _ curNId) =
        GrammarState iGr tGr iGrPos c SelectionDrag curNId

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

newNode :: Coords -> GrammarState -> GrammarState
newNode coords st =
    GrammarState gr' tGr pos (id + 1) lState (Just id)
  where
    id = counter st
    gr = initialGraph st
    (dom, cod, nR, eR) =
        (domain gr, codomain gr, GM.nodeRelation gr, GM.edgeRelation gr)
    gr' = GM.graphMorphism (G.insertNode id dom) cod nR eR
    pos = M.insert id coords $ initialGraphPos st
    lState = leftButtonState st
    tGr = typeGraph st
    
