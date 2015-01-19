import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Data.Foldable
import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import Diagrams.Prelude as D
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
type ObjDiagram = QDiagram Cairo R2 [Obj]
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


defRadius = 0.02 :: Double
defSep = 10 :: Double
--defRadius :: Double -> Double -> Double
--defRadius width height = (max width height) * 0.02

defLineWidth = 2 :: Double
defBorderColor = (0, 0, 0)
neutralColor = (0.8, 0.8, 0.8)
data GraphId = IGraph | TGraph

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
    diagRef <- newIORef mempty

    canvas `on` buttonPressEvent $ mouseClick canvas grBoxRef tGrBoxRef diagRef
--    canvas `on` buttonReleaseEvent $ mouseRelease grBoxRef tGrBoxRef
    canvas `on` sizeRequest $ return (Requisition 600 800)
    canvas `on` exposeEvent $ liftIO $ updateCanvas canvas grBoxRef tGrBoxRef diagRef
    widgetAddEvents canvas [Button3MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas grBoxRef tGrBoxRef diagRef

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

mouseClick :: WidgetClass widget => widget -> IORef EditingBox
           -> IORef EditingBox -> IORef ObjDiagram -> EventM EButton Bool
mouseClick widget grBoxRef tGrBoxRef diagRef = do
    button <- eventButton
    click  <- eventClick
    graph <- liftIO $ readIORef grBoxRef >>= return . eBoxGraph
    tGraph <- liftIO $ readIORef tGrBoxRef >>= return . eBoxGraph
    da <- eventWindow
    width <- liftIO $ drawWindowGetWidth da >>= return . fromIntegral
    height <- liftIO $ drawWindowGetHeight da >>= return . fromIntegral
    coords@(x, y) <- eventCoordinates
    diag <- liftIO $ readIORef diagRef
    grBox <- liftIO $ readIORef grBoxRef
    tGrBox <- liftIO $ readIORef tGrBoxRef
    let normCoords = normalize width height coords
        obj = sample diag (p2 (x, -y))
        grBox' = case (obj, button, click) of
            ([GraphBox], LeftButton, DoubleClick) ->
                EditingBox (newNode normCoords graph) NoMouseAction
            ((GraphBox:Node n:_), RightButton, SingleClick) ->
                grBox { mouseAction = NodeSel n }
            ((GraphBox:Node n:_), LeftButton, SingleClick) ->
                case mouseAction grBox of
                    EdgeCreation s -> 
                        EditingBox (newEdge s n graph) NoMouseAction
                    otherwise -> grBox { mouseAction = EdgeCreation n }
            otherwise -> grBox
        tGrBox' = case (obj, button, click) of
            ([TGraphBox], LeftButton, DoubleClick) ->
                EditingBox (newNode normCoords tGraph) NoMouseAction
            ((TGraphBox:Node n:_), RightButton, SingleClick) ->
                tGrBox { mouseAction = NodeSel n }
            ((TGraphBox:Node n:_), LeftButton, SingleClick) ->
                tGrBox { mouseAction = EdgeCreation n }
            otherwise -> tGrBox
    liftIO $ writeIORef grBoxRef grBox'
    liftIO $ writeIORef tGrBoxRef tGrBox'
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

fitCoords :: Coords -> Coords
fitCoords = (,) <$> fitPos . fst <*> fitPos . snd
  where
    fitPos x
        | x < defRadius = defRadius
        | x > 1 - defRadius = 1 - defRadius
        | otherwise = x


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

 
addMainCallBacks :: GUI -> IORef Grammar -> IO ()
addMainCallBacks gui gramRef = do
    let window   = mainWindow gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
    window `on` objectDestroy $ mainQuit
    iGraphButton `on` buttonActivated $ iGraphDialog gramRef

    return ()

updateCanvas :: DrawingArea -> IORef EditingBox -> IORef EditingBox
             -> IORef ObjDiagram -> IO Bool
updateCanvas canvas grBoxRef tGrBoxRef diagRef = do
    graph <- liftIO $ readIORef grBoxRef >>= return . eBoxGraph
    tGraph <- liftIO $ readIORef tGrBoxRef >>= return . eBoxGraph
    diag <- readIORef diagRef
    da <- widgetGetDrawWindow canvas
    width <- liftIO $ drawWindowGetWidth da >>= return . fromIntegral
    height <- liftIO $ drawWindowGetHeight da >>= return . fromIntegral
    let graphBox = roundedRect width (height / 2) 10
                    # value [GraphBox] # fc lightgreen
        tGraphBox = roundedRect (width / 2) (height / 2) 10
                    # value [TGraphBox] # fc lightyellow
        newDiag = mconcat [
                   drawEdges graph "i",
                   drawNodesSimple graph "i" width height
--                   drawNodes tGraph "i" width height,
--                   D.alignTL $ graphBox === strutY rad === tGraphBox # centerX,
--                   D.alignTL $ rect width height # value []
                  ]
        rad = defRadius * (max width height)
    defaultRender canvas newDiag
    liftIO $ writeIORef diagRef newDiag
    
    return True
        
renderColor :: EColor -> Gtk.Render ()
renderColor (r, g, b) = setSourceRGB r g b

--drawEdges :: Graph -> String -> QDiagram Cairo R2 [Obj] -> Diagram Cairo R2
drawEdges graph prefix =
    mconcat $ map drawEdge pairs
  where
    pairs = G.edges graph >>= (G.nodesConnectedTo graph)
    drawEdge (s, t) =
        connectOutside (prefix ++ show s) (prefix ++ show t)


{-
addNodeValues :: Diagram Cairo R2 -> ObjDiagram
addNodeValues diag =
    mconcat $ map nameToValue $ names diag
  where
    nameToValue (name, _) =
        case lookupName name diag of
            Just s -> s # value Node (read name :: Int)
            otherwise -> mempty
-}

--drawNodesSimple :: Graph -> String -> Double -> Double -> QDiagram Cairo R2 [Obj]
drawNodesSimple graph prefix width height =
    D.position $ map (drawNode graph) $ G.nodes graph
  where
    rad = defRadius * (max width height)
    drawNode graph nId =
        case G.nodePayload graph nId of
            Just ((x, y), color) ->
                ((p2 (x * width, -y * height)),
                 D.circle rad # fc green # named nId)
            otherwise -> ((p2 (0,0)), mempty)

    
drawNodes :: Graph -> String -> Double -> Double -> QDiagram Cairo R2 [Obj]
drawNodes graph prefix width height =
    D.position $ map (drawNode graph) $ G.nodes graph
  where
    rad = defRadius * (max width height)
    drawNode graph nId =
        case G.nodePayload graph nId of
            Just ((x, y), color) ->
                ((p2 (x * width, -y * height)),
                 D.circle rad # fc green # value [Node nId] # named nId)
            otherwise -> ((p2 (0,0)), mempty)

{-
drawNodes gram graphId = do
    setLineWidth defLineWidth
    let gr = graph graphId
    mapM_ (drawNode gr) $ G.nodes gr
  where
    graph TGraph = GG.typeGraph gram
    graph IGraph = M.domain . GG.initialGraph $ gram
    drawNode gr nId =
        case G.nodePayload gr nId of
            Just ((x, y), color) -> do
                                    renderColor defBorderColor
                                    Gtk.arc x y defRadius 0 $ 2 * pi
                                    strokePreserve
                                    renderColor color
                                    fill
            otherwise -> return ()
-}



mouseRelease :: IORef EditingBox -> IORef EditingBox -> EventM EButton Bool
mouseRelease grBoxRef tGrBoxRef = do
    liftIO $ do modifyIORef grBoxRef $ cancelDrag
                modifyIORef tGrBoxRef $ cancelDrag
    return True
  where
    cancelDrag box = box { mouseAction = NoMouseAction }

