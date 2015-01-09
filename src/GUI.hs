import Control.Monad.Trans.Class (lift)
import Graph
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

main = do
    initGUI
    window <- windowNew
    set window [ windowTitle := "Verigraph" ]
    window `on` objectDestroy $ mainQuit

    canvas <- drawingAreaNew
    canvas `on` sizeRequest $ return (Requisition 40 40)
    ctxt <- cairoCreateContext Nothing
    canvas `on` draw $ updateCanvas canvas
    canvas `on` buttonPressEvent $ mouseClick

    containerAdd window canvas

    widgetShowAll window
    widgetShow canvas
    mainGUI 


updateCanvas :: WidgetClass widget => widget -> Render ()
updateCanvas canvas = do
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
    setSourceRGB 1 0 0
    setLineWidth 20
    arc width height 20 0 $ 2 * pi
    strokePreserve
    fill

mouseClick :: EventM EButton Bool
mouseClick = do
    button <- eventButton
    liftIO $ case button of
                LeftButton   -> putStrLn "LeftButton"
                MiddleButton -> putStrLn "MiddleButton"
                RightButton  -> putStrLn "RightButton"
                otherwise    -> putStrLn "Unknown button"
    return True

