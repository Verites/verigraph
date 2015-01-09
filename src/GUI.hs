import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import Data.IORef
import qualified Data.Map as M
import Data.Foldable
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Prelude hiding (mapM_)

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
    setSourceRGB 1 0 0
    setLineWidth 2
    let posMap = initialGraphPos st
    mapM_ drawNode posMap
  where
    drawNode (x, y) = do
        arc x y 20 0 $ 2 * pi
        strokePreserve
        setSourceRGB 1 1 0
        fill
    

mouseClick :: GUI -> IORef GrammarState -> EventM EButton Bool
mouseClick gui st = do
    button <- eventButton
    click  <- eventClick
    coords@(x, y) <- eventCoordinates
    case (button, click) of
        (LeftButton, _) -> liftIO $ do modifyIORef st (newNode coords)
                                       widgetQueueDraw $ mainWindow gui
        otherwise                 -> liftIO $ putStrLn "Unknown button"
    return True

newNode :: Coords -> GrammarState -> GrammarState
newNode coords st =
    GrammarState gr' pos (id + 1)
  where
    id = counter st
    gr = initialGraph st
    gr' = G.insertNode id gr
    k = "i" ++ (show id)
    pos = M.insert k coords $ initialGraphPos st
    
