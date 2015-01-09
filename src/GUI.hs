import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import Data.IORef
import qualified Data.Map as M
import Data.Foldable
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Prelude hiding (mapM_, any)

type Coords = (Double, Double)

data GUI = GUI {
    mainWindow    :: Window,
    initialCanvas :: DrawingArea
    }

data GrammarState = GrammarState {
    initialGraph    :: G.Graph String String,
    initialGraphPos :: M.Map String Coords,
    counter         :: Int
    }

radius = 20 :: Double

main = do
    initGUI
    st <- newIORef $ GrammarState G.empty M.empty 0
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
        otherwise                 -> liftIO $ putStrLn "Unknown button"
    return True

leftDoubleClick gui st coords = do
    state <- liftIO $ readIORef st
    let posMap = initialGraphPos state
    if isOverAnyNode coords posMap
    then putStrLn $ "clicked over node" ++ (show coords)
    else do modifyIORef st (newNode coords)
            widgetQueueDraw $ mainWindow gui
  where
    isOverAnyNode coords posMap =
        any (isOverNode coords) (M.elems posMap)
    isOverNode (x, y) (nx, ny) = -- consider square box for simplicity
        abs (nx - x) <= 2 * radius &&
        abs (ny - y) <= 2 * radius
    

newNode :: Coords -> GrammarState -> GrammarState
newNode coords st =
    GrammarState gr' pos (id + 1)
  where
    id = counter st
    gr = initialGraph st
    gr' = G.insertNode id gr
    k = "i" ++ (show id)
    pos = M.insert k coords $ initialGraphPos st
    
