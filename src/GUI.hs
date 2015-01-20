import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.Maybe (fromJust, isJust, isNothing)
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
data Obj = Node Int | Edge Int
    deriving (Show, Eq)
type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload

data RNode = GNode Graph G.NodeId
data RGraph = RGraph Graph

class Renderable a where
    render :: a -> Render ()

instance Renderable (RNode) where
    render (GNode g n) =
        case G.nodePayload g n of
            Just ((x, y), color) -> do
                                    setLineWidth defLineWidth
                                    renderColor defBorderColor
                                    Gtk.arc x y defRadius 0 $ 2 * pi
                                    strokePreserve
                                    renderColor color
                                    fill
            otherwise -> return ()

instance Renderable (RGraph) where
    render (RGraph g) = mapM_ (render . GNode g) $ G.nodes g


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

    let graph = M.domain $ GG.initialGraph gram
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
    canvas `on` draw $ updateCanvas canvas grBoxRef
    widgetAddEvents canvas [Button3MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas grBoxRef

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
    let obj = fetchObj graph coords
        grBox' = case (obj, button, click) of
            (Nothing, LeftButton, DoubleClick) ->
                EditingBox (newNode coords graph) NoMouseAction
            (Just (Node n), RightButton, SingleClick) ->
                grBox { mouseAction = NodeSel n }
            (Just (Node n), LeftButton, SingleClick) ->
                case mouseAction grBox of
                    EdgeCreation s -> 
                        EditingBox (newEdge s n graph) NoMouseAction
                    otherwise -> grBox { mouseAction = EdgeCreation n }
            otherwise -> grBox
    liftIO $ writeIORef grBoxRef grBox'
    liftIO $ widgetQueueDraw widget
    return True

fetchObj :: Graph -> Coords -> Maybe Obj
fetchObj graph coords
    | not . null $ fetchNodes = Just . Node . head $ fetchNodes
--    | not . null $ fetchEdges = Just . Node . head $ fetchEdges
    | otherwise = Nothing
  where
    fetchNodes = filter (overNode graph coords) $ G.nodes graph

overNode :: Graph -> Coords -> G.NodeId -> Bool
overNode graph coords n
    | isNothing p = False
    | distance coords nCoords < defRadius = True
    | otherwise = False
  where
    p = G.nodePayload graph n
    nCoords = fst $ fromJust p
    
distance :: Coords -> Coords -> Double
distance (x0, y0) (x1, y1) =
    sqrt $ (square (x1 - x0)) + (square (y1 - y0))
  where
    square x = x * x

normalize :: Double -> Double -> Coords -> Coords
normalize width height coords@(x, y)
    | height == 0 || width == 0 = coords
    | otherwise = (x / width, y / height)


newNode :: Coords -> Graph -> Graph
newNode coords graph =
    G.insertNodeWithPayload newId graph (coords, neutralColor)
  where
    newId = length . G.nodes $ graph

newEdge :: G.NodeId -> G.NodeId -> Graph -> Graph
newEdge src tgt graph =
    G.insertEdge edgeId src tgt graph
  where
    edgeId = length . G.edges $ graph

mouseMove :: WidgetClass widget
          => widget -> IORef EditingBox -> EventM EMotion Bool
mouseMove canvas grBoxRef = do
    grBox <- liftIO $ readIORef grBoxRef
    coords@(x, y) <- eventCoordinates
    let graph = eBoxGraph grBox
        grMouse = mouseAction grBox
    case grMouse of
       (NodeSel n) ->
            let graph' =
                    G.insertNodeWithPayload n graph (coords, neutralColor)
            in liftIO $ writeIORef grBoxRef $ grBox { eBoxGraph = graph' }
       otherwise -> return ()

    liftIO $ widgetQueueDraw canvas
    return True

 
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
    render (RGraph graph)
    
        
renderColor :: EColor -> Gtk.Render ()
renderColor (r, g, b) = setSourceRGB r g b

mouseRelease :: IORef EditingBox -> IORef EditingBox -> EventM EButton Bool
mouseRelease grBoxRef tGrBoxRef = do
    liftIO $ do modifyIORef grBoxRef $ cancelDrag
                modifyIORef tGrBoxRef $ cancelDrag
    return True
  where
    cancelDrag box = box { mouseAction = NoMouseAction }

