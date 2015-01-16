import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Data.Foldable
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import qualified Morphism as M
import Prelude hiding (mapM_, any)

type Coords = (Double, Double)
type EColor = (Double, Double, Double)
type NodePayload = (Coords, EColor)
type EdgePayload = EColor
type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload


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


radius = 20 :: Double
lineWidth = 2 :: Double
borderColor = (0, 0, 0)
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
    let gId = IGraph
        tId = TGraph
    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog

    frame <- frameNew
    frameSetLabel frame "Initial Graph"
    canvas <- drawingAreaNew
    containerAdd frame canvas

    typeFrame  <- frameNew
    typeCanvas <- drawingAreaNew
    containerAdd typeFrame typeCanvas
    frameSetLabel typeFrame "T Graph"
    canvas `on` buttonPressEvent $ mouseClick dialog gramRef gId
    canvas `on` sizeRequest $ return (Requisition 40 40)
    canvas `on` exposeEvent $ liftIO $ updateCanvas canvas gramRef gId 

    typeCanvas `on` buttonPressEvent $ mouseClick dialog gramRef tId
    typeCanvas `on` sizeRequest $ return (Requisition 40 40)
{-
    canvas `on` buttonReleaseEvent $ mouseRelease st gId
    widgetAddEvents canvas [Button1MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas st gId

 --   typeCanvas `on` draw $ updateCanvas typeCanvas st tId
    typeCanvas `on` buttonReleaseEvent $ mouseRelease st tId
    widgetAddEvents typeCanvas [Button1MotionMask]
    typeCanvas `on` motionNotifyEvent $ mouseMove typeCanvas st tId
-}


    let cArea = castToBox contentArea
    boxPackStart cArea frame PackGrow 1
    boxPackStart cArea typeFrame PackGrow 1
    widgetShowAll dialog
    dialogRun dialog
    return ()

mouseClick :: WidgetClass widget
           => widget -> IORef Grammar -> GraphId -> EventM EButton Bool
mouseClick widget gramRef graphId = do
    button <- eventButton
    click  <- eventClick
    gram <- liftIO $ readIORef gramRef
    drawWindow <- eventWindow
    width <- liftIO $ drawWindowGetWidth drawWindow
    height <- liftIO $ drawWindowGetHeight drawWindow
    coords@(x, y) <- eventCoordinates
    let normCoords = normalize width height coords
        newGram = case (button, click) of
            (LeftButton, DoubleClick) -> leftDoubleClick gram graphId coords
--            (LeftButton, SingleClick) -> leftSingleClick gram graphId coords
            otherwise                 -> gram

    liftIO $ writeIORef gramRef newGram
    liftIO $ widgetQueueDraw widget
    return True

normalize :: Int -> Int -> Coords -> Coords
normalize width height coords@(x, y)
    | height == 0 || width == 0 = coords
    | otherwise = (x / (fromIntegral width), y / (fromIntegral height))

leftDoubleClick :: Grammar -> GraphId -> Coords -> Grammar
leftDoubleClick gram IGraph coords =
    GG.graphGrammar graph' (GG.typeGraph gram) (GG.rules gram)
  where
    graph = GG.initialGraph gram
    (dom, cod, nR, eR) =
        (M.domain graph, M.codomain graph, GM.nodeRelation graph, GM.edgeRelation graph)
    graph' = GM.graphMorphism (newNode coords dom) cod nR eR

leftDoubleClick gram TGraph coords =
    GG.graphGrammar (GG.initialGraph gram) tGraph' (GG.rules gram)
  where
    tGraph' = newNode coords $ GG.typeGraph gram

newNode :: Coords -> Graph -> Graph
newNode coords graph =
    G.insertNodeWithPayload newId graph (coords, neutralColor)
  where
    newId = (+1) . length . G.nodes $ graph

{-
    case graph graphId of
        Nothing -> state
        Just grState ->
            let posMap = graphPos grState
            in if isOverAnyNode coords posMap
                then state
                else newNode graphId coords state
-}


{-
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


updateCanvas :: WidgetClass widget
             => widget -> IORef Grammar -> GraphId -> IO Bool
updateCanvas canvas gramRef graphId = do
{-
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
-}
    da <- widgetGetDrawWindow canvas
    gram <- liftIO $ readIORef gramRef
    renderWithDrawable da $ drawNodes gram graphId
    return True

renderColor :: EColor -> Render ()
renderColor (r, g, b) = setSourceRGB r g b

drawNodes :: Grammar -> GraphId -> Render ()
drawNodes gram graphId = do
    setLineWidth lineWidth
    let gr = graph graphId
    mapM_ (drawNode gr) $ G.nodes gr
  where
    graph TGraph = GG.typeGraph gram
    graph IGraph = M.domain . GG.initialGraph $ gram
    drawNode gr nId =
        case G.nodePayload nId gr of
            Just ((x, y), color) -> do
                                    renderColor borderColor
                                    arc x y radius 0 $ 2 * pi
                                    strokePreserve
                                    renderColor color
                                    fill
            otherwise -> return ()


{-

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
    
-}
