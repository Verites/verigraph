import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.IORef
import qualified Data.Map as M
import Data.Foldable
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Morphism
import Prelude hiding (mapM_, any)

type Coords = (Double, Double)

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

data GrammarState = GrammarState {
    getGrammar :: GG.GraphGrammar String String,
    graphStateMap :: M.Map Int GraphState,
    grammarCounter :: Int, -- id's from graphStates
    leftButtonState :: LeftButtonState
    }

data GraphState = GraphState {
    graphCounter :: Int,
    graphPos :: M.Map G.NodeId Coords
    }

radius = 20 :: Double
lineWidth = 2 :: Double

main = do
    initGUI
    let iSimpleGraph = G.empty :: G.Graph String String
        tGraph = G.empty :: G.Graph String String
        iGraph = GM.empty iSimpleGraph tGraph
        iGraphState = GraphState 0 M.empty
        tGraphState = GraphState 0 M.empty
        grammar = GG.graphGrammar tGraph iGraph []
        graphStates = M.insert 0 iGraphState $
                      M.insert 1 tGraphState $
                      M.empty
        grammarState = GrammarState grammar graphStates 2 LeftButtonFree
                    
    st <- newIORef grammarState
--    st <- newIORef $ GraphState iGraph M.empty 0 LeftButtonFree Nothing
    gui <- createGUI
    addMainCallBacks gui st 0
--    addCallBacks gui st
    --ctxt <- cairoCreateContext Nothing
    widgetShowAll $ mainWindow gui
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

iGraphDialog :: IORef GrammarState -> Int ->  IO ()
iGraphDialog st gId = do
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
    canvas `on` draw $ updateCanvas canvas st gId 
    canvas `on` buttonPressEvent $ mouseClick dialog st gId
    canvas `on` buttonReleaseEvent $ mouseRelease st gId
    widgetAddEvents canvas [Button1MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas st gId

    let cArea = castToBox contentArea
    boxPackStart cArea frame PackGrow 1
--    boxPackStart cArea typeButton PackGrow 1
    widgetShowAll dialog
    putStrLn "iGraphDialog"
    dialogRun dialog
    return ()
   

addMainCallBacks :: GUI -> IORef GrammarState -> Int -> IO ()
addMainCallBacks gui st gId = do
    let window   = mainWindow gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
    window `on` objectDestroy $ mainQuit
    iGraphButton `on` buttonActivated $ iGraphDialog st gId

    return ()

updateCanvas :: WidgetClass widget
             => widget -> IORef GrammarState -> Int -> Render ()
updateCanvas canvas st gId = do
    width'  <- liftIO $ widgetGetAllocatedWidth canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width = realToFrac width' / 2
        height = realToFrac height' / 2
    drawNodes st gId width height

drawNodes :: IORef GrammarState -> Int -> Double -> Double -> Render ()
drawNodes state gId x y = do
    st <- liftIO $ readIORef state
    setLineWidth lineWidth
    let graphState = M.lookup gId $ graphStateMap st
    case graphState of
        Just grState -> mapM_ drawNode $ graphPos grState
        otherwise -> return ()
  where
    drawNode (x, y) = do
        setSourceRGB 0 0 0
        arc x y radius 0 $ 2 * pi
        strokePreserve
        setSourceRGB 0.8 0.8 0.8
        fill

mouseClick :: WidgetClass widget
           => widget -> IORef GrammarState -> Int -> EventM EButton Bool
mouseClick widget st gId = do
    button <- eventButton
    click  <- eventClick
    state <- liftIO $ readIORef st
    coords@(x, y) <- eventCoordinates
    let newState = case (button, click) of
            (LeftButton, DoubleClick) -> leftDoubleClick state gId coords
            (LeftButton, SingleClick) -> leftSingleClick state gId coords
            otherwise                 -> state
    liftIO $ writeIORef st newState
    liftIO $ widgetQueueDraw widget
    return True

mouseRelease :: IORef GrammarState -> Int -> EventM EButton Bool
mouseRelease st gId = do
    liftIO $ modifyIORef st cancelDrag
    return True
  where
    cancelDrag (GrammarState grammar grStates grCounter _) =
        GrammarState grammar grStates grCounter LeftButtonFree

mouseMove :: WidgetClass widget
          => widget -> IORef GrammarState -> Int -> EventM EMotion Bool
mouseMove widget st gId = do
    state <- liftIO $ readIORef st
    coords <- eventCoordinates
    let graphState = M.lookup gId $ graphStateMap state
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
    newGraphState (GraphState c grPos) nId newCoords =
        GraphState c (M.insert nId newCoords grPos)
    updateCoords grState nId newCoords st@(GrammarState gram grStates c lB) =
        GrammarState gram
                     (M.insert gId (newGraphState grState nId newCoords) grStates)
                     c lB

leftDoubleClick :: GrammarState -> Int -> Coords -> GrammarState
leftDoubleClick state gId coords = do
    case graphState of
        Nothing -> state
        Just grState -> do
            let posMap = graphPos grState
            if isOverAnyNode coords posMap
            then state
            else newNode 0 coords state
  where
    graphState = M.lookup gId $ graphStateMap state


leftSingleClick :: GrammarState -> Int -> Coords -> GrammarState
leftSingleClick state gId coords = do
    case graphState of
        Nothing -> state
        Just grState -> do
            let posMap = graphPos grState
            case nodeId posMap of
                Just k -> nodeDrag k state
                otherwise -> selDrag state
  where
    graphState = M.lookup gId $ graphStateMap state
    nodeId posMap = checkNodeClick coords posMap
    nodeDrag newId (GrammarState iGr iGrPos c _) =
        GrammarState iGr iGrPos c (NodeDrag newId)
    selDrag (GrammarState iGr iGrPos c _) =
        GrammarState iGr iGrPos c SelectionDrag

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

newNode :: Int -> Coords -> GrammarState -> GrammarState
newNode gId coords st@(GrammarState gram grStates c lB) = -- for now only initial graph. 
                                                             -- Will change to a safer, 
                                                             -- type base approach
    GrammarState gram' grStates' c lB
  where
    (Just grState) = M.lookup gId $ graphStateMap st -- FIX unsafe
    (tGraph, gr, rs) = (GG.typeGraph gram, GG.initialGraph gram, GG.rules gram)
    (dom, cod, nR, eR) =
        (domain gr, codomain gr, GM.nodeRelation gr, GM.edgeRelation gr)
    gr' = GM.graphMorphism (G.insertNode nId dom) cod nR eR
    (nId, grPos) =
        (graphCounter grState, graphPos grState)
    grPos' = M.insert nId coords grPos
    grState' = GraphState (nId + 1) grPos'
    grStates' = M.insert gId grState' grStates
    gram' = GG.graphGrammar tGraph gr' rs
    
