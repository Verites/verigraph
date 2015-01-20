import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Data.Foldable
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Gtk
import Graphics.UI.Gtk.Gdk.EventM
import qualified Morphism as M
import Prelude hiding (mapM_, any)

type Coords = (Double, Double)
type EColor = (Double, Double, Double)
type NodePayload = (Coords, EColor)
type EdgePayload = EColor
data Obj = Node Int | Edge Int | GraphBox | TGraphBox
    deriving (Show, Eq)
type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload


data MouseAction = EdgeCreation Int | NodeSel Int | NoMouseAction
    deriving (Show)

data Buttons = Buttons {
    editInitialGraph :: Button,
    addRule :: Button
    }

data EditingBox = EditingBox {
    eBoxGraph :: Graph,
    mouseAction :: MouseAction
    }
    

data GUI = GUI {
    mainWindow    :: Window,
    buttons :: Buttons,
    canvas :: DrawingArea
    }


defRadius = 20 :: Double
--defRadius :: Double -> Double -> Double
--defRadius width height = (max width height) * 0.02

defLineWidth = 2 :: Double
defBorderColor = (0, 0, 0)
neutralColor = (0.8, 0.8, 0.8)

main = do
    initGUI
    let grammar :: Grammar
        grammar = GG.graphGrammar (GM.empty G.empty G.empty) G.empty []
    gramRef <- newIORef grammar
    gui <- createGUI
    widgetShowAll $ mainWindow gui
    addMainCallBacks gui gramRef
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

iGraphDialog :: IORef Grammar -> IO ()
iGraphDialog gramRef = do
    gram <- readIORef gramRef

    let gId = IGraph
        tId = TGraph
        graph = M.domain $ GG.initialGraph gram
        tGraph = GG.typeGraph gram
        grBox = EditingBox graph NoMouseAction
        tGrBox = EditingBox tGraph NoMouseAction
    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog

    frame <- frameNew
    frameSetLabel frame "Initial Graph"
    canvas <- drawingAreaNew
    containerAdd frame canvas
    grBoxRef <- newIORef grBox
    tGrBoxRef <- newIORef tGrBox

    canvas `on` buttonPressEvent $ mouseClick canvas grBoxRef
--    canvas `on` buttonReleaseEvent $ mouseRelease grBoxRef tGrBoxRef
    canvas `on` sizeRequest $ return (Requisition 600 800)
    canvas `on` exposeEvent $ liftIO $ updateCanvas canvas grBoxRef
    widgetAddEvents canvas [Button3MotionMask]
--    canvas `on` motionNotifyEvent $ mouseMove canvas grBoxRef

{-
    tFrame  <- frameNew
    tCanvas <- drawingAreaNew
    containerAdd tFrame tCanvas
    frameSetLabel tFrame "T Graph"
    
    tEGuiRef <- newIORef tEGui
    tGraphRef <- newIORef tGraph
    tCanvas `on` buttonPressEvent $ mouseClick tCanvas tEGuiRef tGraphRef
    tCanvas `on` sizeRequest $ return (Requisition 40 40)
    tCanvas `on` exposeEvent $ liftIO $ updateCanvas tCanvas tEGuiRef tGraphRef
    widgetAddEvents tCanvas [Button1MotionMask]
    tCanvas `on` motionNotifyEvent $ mouseMove tCanvas tEGuiRef tGraphRef
-}

{-
    canvas `on` buttonReleaseEvent $ mouseRelease st gId

 --   typeCanvas `on` draw $ updateCanvas typeCanvas st tId
    typeCanvas `on` buttonReleaseEvent $ mouseRelease st tId
    widgetAddEvents typeCanvas [Button1MotionMask]
    typeCanvas `on` motionNotifyEvent $ mouseMove typeCanvas st tId
-}


    let cArea = castToBox contentArea
    boxPackStart cArea frame PackGrow 1
--    boxPackStart cArea tFrame PackGrow 1
    widgetShowAll dialog
    dialogRun dialog
    return ()

mouseClick :: WidgetClass widget
           => widget -> IORef EditingBox -> EventM EButton Bool
mouseClick widget grBoxRef = do
    button <- eventButton
    click  <- eventClick
    graph <- liftIO $ readIORef grBoxRef >>= return . eBoxGraph
    coords@(x, y) <- eventCoordinates
    grBox <- liftIO $ readIORef grBoxRef
    let grBox' = case (button, click) of
            (LeftButton, DoubleClick) ->
                EditingBox (newNode coords graph) NoMouseAction
{-
            ((GraphBox:Node n:_), RightButton, SingleClick) ->
                grBox { mouseAction = NodeSel n }
            ((GraphBox:Node n:_), LeftButton, SingleClick) ->
                case mouseAction grBox of
                    EdgeCreation s -> 
                        EditingBox (newEdge s n graph) NoMouseAction
                    otherwise -> grBox { mouseAction = EdgeCreation n }
-}
            otherwise -> grBox
    liftIO $ writeIORef grBoxRef grBox'
    liftIO $ widgetQueueDraw widget
    return True

normalize :: Double -> Double -> Coords -> Coords
normalize width height coords@(x, y)
    | height == 0 || width == 0 = coords
    | otherwise = (x / width, y / height)


newNode :: Coords -> Graph -> Graph
newNode coords graph =
    G.insertNodeWithPayload newId graph (fitCoords coords, neutralColor)
  where
    newId = length . G.nodes $ graph

newEdge :: G.NodeId -> G.NodeId -> Graph -> Graph
newEdge src tgt graph =
    G.insertEdge edgeId src tgt graph
  where
    edgeId = length . G.edges $ graph

{-
mouseMove :: WidgetClass widget => widget -> IORef EditingBox
          -> IORef EditingBox -> IORef ObjDiagram -> EventM EMotion Bool
mouseMove canvas grBoxRef tGrBoxRef diagRef = do
    grBox <- liftIO $ readIORef grBoxRef
    tGrBox <- liftIO $ readIORef tGrBoxRef
    diag <- liftIO $ readIORef diagRef
    da <- eventWindow
    width <- liftIO $ drawWindowGetWidth da >>= return . fromIntegral
    height <- liftIO $ drawWindowGetHeight da >>= return . fromIntegral
    coords@(x, y) <- eventCoordinates
    let graph = eBoxGraph grBox
        tGraph = eBoxGraph tGrBox
        normCoords = fitCoords $ normalize width height coords
        obj = sample diag (p2 (x, -y))
        grMouse = mouseAction grBox
        tGrMouse = mouseAction tGrBox
    case (obj, grMouse, tGrMouse) of
       ((GraphBox:_), NodeSel n, _) ->
            let graph' =
                    G.insertNodeWithPayload n graph (normCoords, neutralColor)
            in liftIO $ writeIORef grBoxRef $ grBox { eBoxGraph = graph' }
       ((TGraphBox:_), _, NodeSel n) ->
            let tGraph' =
                    G.insertNodeWithPayload n tGraph (normCoords, neutralColor)
            in liftIO $ writeIORef tGrBoxRef $ tGrBox { eBoxGraph = tGraph' }
       otherwise -> return ()

    liftIO $ widgetQueueDraw canvas
    return True
-}

 
addMainCallBacks :: GUI -> IORef Grammar -> IO ()
addMainCallBacks gui gramRef = do
    let window   = mainWindow gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
    window `on` objectDestroy $ mainQuit
    iGraphButton `on` buttonActivated $ iGraphDialog gramRef

    return ()

updateCanvas :: DrawingArea -> IORef EditingBox -> Render ()
updateCanvas canvas grBoxRef = do
    graph <- liftIO $ readIORef grBoxRef >>= return . eBoxGraph
    drawNodes graph
    
drawNodes :: Graph -> Render ()
drawNodes graph = do
    setLineWidth defLineWidth
    mapM_ (drawNode graph) $ G.nodes graph
  where
    drawNode gr nId =
        case G.nodePayload gr nId of
            Just ((x, y), color) -> do
                                    renderColor defBorderColor
                                    Gtk.arc x y defRadius 0 $ 2 * pi
                                    strokePreserve
                                    renderColor color
                                    fill
            otherwise -> return ()
        
renderColor :: EColor -> Gtk.Render ()
renderColor (r, g, b) = setSourceRGB r g b

mouseRelease :: IORef EditingBox -> IORef EditingBox -> EventM EButton Bool
mouseRelease grBoxRef tGrBoxRef = do
    liftIO $ do modifyIORef grBoxRef $ cancelDrag
                modifyIORef tGrBoxRef $ cancelDrag
    return True
  where
    cancelDrag box = box { mouseAction = NoMouseAction }

