import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import Data.IORef
import qualified Data.Map as M
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

type Coords = (Double, Double)

data GrammarState = GrammarState {
    initialGraph    :: G.Graph String String,
    initialGraphPos :: M.Map String Coords,
    counter         :: Int
    }

main = do
    initGUI
    st <- newIORef $ GrammarState G.empty M.empty 0
    window <- windowNew
    set window [ windowTitle := "Verigraph" ]
    window `on` objectDestroy $ mainQuit

    canvas <- drawingAreaNew
    canvas `on` sizeRequest $ return (Requisition 40 40)
    ctxt <- cairoCreateContext Nothing
    canvas `on` draw $ updateCanvas canvas st
    canvas `on` buttonPressEvent $ mouseClick st

    containerAdd window canvas

    widgetShowAll window
    widgetShow canvas
    mainGUI 


updateCanvas :: WidgetClass widget
             => widget
             -> IORef GrammarState
             -> Render ()
updateCanvas canvas st = do
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
    setSourceRGB 1 0 0
    setLineWidth 20
    arc width height 20 0 $ 2 * pi
    strokePreserve
    fill

mouseClick :: IORef GrammarState -> EventM EButton Bool
mouseClick st = do
    button <- eventButton
    click  <- eventClick
    coords@(x, y) <- eventCoordinates
    case (button, click) of
        (LeftButton, DoubleClick) -> liftIO $ modifyIORef st (newNode coords)
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
    
