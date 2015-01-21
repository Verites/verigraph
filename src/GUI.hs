module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import Data.Colour.Names
import Data.Colour.SRGB (Colour, toSRGB, RGB (..))
import Data.Colour.Palette.ColorSet (Kolor, webColors, infiniteWebColors)
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
import qualified Relation as R

defRadius = 20 :: Double
defLineWidth = 2 :: Double
defBorderColor = black
neutralColor = gray

type Coords = (Double, Double)
type NodePayload = (Coords, Kolor)
type EdgePayload = Kolor
data Obj = Node Int | Edge Int
    deriving (Show, Eq)
type Grammar = GG.GraphGrammar NodePayload EdgePayload
type Graph = G.Graph NodePayload EdgePayload
type GraphMorphism = GM.GraphMorphism NodePayload EdgePayload

data RNode = RNode Graph G.NodeId
data REdge = REdge Graph G.EdgeId
data RGraph = RGraph Graph

class Renderable a where
    render :: a -> Render ()

instance Renderable (RNode) where
    render (RNode g n) =
        case G.nodePayload g n of
            Just ((x, y), color) -> do
                                    setLineWidth defLineWidth
                                    renderColor defBorderColor
                                    Gtk.arc x y defRadius 0 $ 2 * pi
                                    strokePreserve
                                    renderColor color
                                    fill
            otherwise -> return ()


instance Renderable (REdge) where
    render (REdge g e)
        | null connected = return ()
        | isNothing srcP || isNothing tgtP = return ()
        | isLoop = drawLoop
        | otherwise = drawEdge
      where
        connected = G.nodesConnectedTo g e
        srcP = G.nodePayload g . fst . head $ connected
        tgtP = G.nodePayload g . snd . head $ connected
        srcCoords@((x, y)) = fst . fromJust $ srcP 
        tgtCoords@((x', y')) = fst . fromJust $ tgtP
        isLoop = srcCoords == tgtCoords
        dirVect@(dirX, dirY) = directionVect srcCoords tgtCoords
        (dx, dy) = (dirX * defRadius, dirY * defRadius)
        drawHead len = do
            relMoveTo (len / 2) 0
            relLineTo (-len / 2) len
            relLineTo (-len / 2) (-len)
        drawLoop = do
            setLineWidth defLineWidth
            renderColor defBorderColor
            Gtk.arcNegative x
                            (y - (2 * defRadius))
                            (1.5 * defRadius)
                            (0.2 * pi)
                            (0.75 * pi)
            identityMatrix
            moveTo x y
            relMoveTo (1.2 * defRadius) (-1.2 * defRadius)
            rotate (pi / 4)
            drawHead $ 0.5 * defRadius
            identityMatrix
            stroke
--            relMoveTo (-1.5 * defRadius) (-1.5 * defRadius)
            
        drawEdge = do
            setLineWidth defLineWidth
            renderColor defBorderColor
            moveTo x y
            relMoveTo (1.5 * dx) (1.5 * dy)
            lineTo (x' - (2 * dx)) (y' - (2 * dy))
            rotate $ -(angle (dirX, dirY))
            drawHead $ 0.5 * defRadius
            identityMatrix
            stroke

instance Renderable (RGraph) where
    render (RGraph g) = do
        mapM_ (render . RNode g) $ G.nodes g
        mapM_ (render . REdge g) $ G.edges g

data QElem = DomNode Int | DomEdge Int | CodNode Int | CodEdge Int
    deriving (Show)

data MouseAction = EdgeCreation QElem | NodeSel QElem | NoMouseAction
    deriving (Show)

data Buttons = Buttons {
    editInitialGraph :: Button,
    addRule :: Button
    }

data EditingBox = EditingBox {
    eBoxGraphMorphism :: GM.TypedGraph NodePayload EdgePayload,
    mouseAction :: MouseAction
    }
    

data GUI = GUI {
    mainWindow    :: Window,
    buttons :: Buttons,
    canvas :: DrawingArea
    }



directionVect :: Coords -> Coords -> Coords
directionVect s@(x, y) t@(x', y')
    | dist == 0 = (0, 0)
    | otherwise = (dx / dist, dy / dist)
  where 
    dist = norm s t
    dx = x' - x
    dy = y' - y

norm :: Coords -> Coords -> Double
norm (x, y) (x', y') =
    sqrt $ (square (x' - x)) + (square (y' - y))
  where
    square x = x * x

angle :: Coords -> Double
angle (dx, dy)
    | dx > 0 = acos dy
    | dx < 0 = - acos dy


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

showGUI = widgetShowAll . mainWindow

iGraphDialog :: IORef Grammar -> IO ()
iGraphDialog gramRef = do
    gram <- readIORef gramRef

    let graph = M.domain $ GG.initialGraph gram
        tGraph = GG.typeGraph gram
        nR = R.empty (G.nodes graph) (G.nodes tGraph)
        eR = R.empty (G.edges graph) (G.edges tGraph)
        grBox = EditingBox
                (GM.graphMorphism graph tGraph nR eR) NoMouseAction
    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog >>= return . castToBox

    frame <- frameNew
    frameSetLabel frame "Initial Graph"
    canvas <- drawingAreaNew
    containerAdd frame canvas
    grBoxRef <- newIORef grBox
    tFrame  <- frameNew
    frameSetLabel tFrame "T Graph"
    tCanvas <- drawingAreaNew
    containerAdd tFrame tCanvas

    canvas `on` buttonPressEvent $ domClick canvas tCanvas grBoxRef
    canvas `on` draw $ updateCanvas canvas grBoxRef M.domain
    widgetAddEvents canvas [Button3MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas grBoxRef

    
    tCanvas `on` buttonPressEvent $ codClick tCanvas canvas grBoxRef
    tCanvas `on` draw $ updateCanvas canvas grBoxRef M.codomain
    widgetAddEvents tCanvas [Button3MotionMask]
    tCanvas `on` motionNotifyEvent $ mouseMove tCanvas grBoxRef

    boxPackStart contentArea frame PackGrow 1
    boxPackStart contentArea tFrame PackGrow 1
    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Apply" ResponseApply

    widgetSetSizeRequest dialog 800 600
    widgetShowAll dialog
    response <- dialogRun dialog
    case response of
        ResponseApply -> do putStrLn $ "changes to initial graph applied"
                            widgetDestroy dialog
        otherwise -> do putStrLn $ "changes to initial graph cancelled"
                        widgetDestroy dialog
    return ()

domClick :: WidgetClass widget
         => widget -> widget -> IORef EditingBox -> EventM EButton Bool
domClick widget codWidget grBoxRef = do
    button <- eventButton
    click  <- eventClick
    grBox <- liftIO $ readIORef grBoxRef
    coords@(x, y) <- eventCoordinates
    let grMorph = eBoxGraphMorphism grBox
        graph = M.domain grMorph
        codGraph = M.codomain grMorph
        obj = fetchObj graph coords
        grBox' = case (obj, button, click) of
            (Nothing, LeftButton, DoubleClick) ->
                EditingBox
                    (GM.updateDomain (newNode coords (\_ -> neutralColor) graph) grMorph)
                    NoMouseAction
            (Just (Node n), RightButton, SingleClick) ->
                grBox { mouseAction = NodeSel (DomNode n) }
            (Just (Node n), LeftButton, SingleClick) ->
                case mouseAction grBox of
                    EdgeCreation (DomNode s) -> 
                        EditingBox
                            (GM.updateDomain (newEdge s n graph) grMorph)
                            NoMouseAction
                    EdgeCreation (CodNode t) ->
                        let domGraph' = attributeTypeColor n graph t codGraph
                            grMorph' = GM.updateDomain domGraph' grMorph
                        in EditingBox (GM.updateNodes n t grMorph') NoMouseAction
                    otherwise -> grBox {mouseAction = EdgeCreation (DomNode n)}
            otherwise -> grBox
    liftIO $ writeIORef grBoxRef grBox'
    liftIO $ widgetQueueDraw widget
    liftIO $ widgetQueueDraw codWidget
    return True

codClick :: WidgetClass widget
         => widget -> widget -> IORef EditingBox -> EventM EButton Bool
codClick widget domWidget grBoxRef = do
    button <- eventButton
    click  <- eventClick
    grBox <- liftIO $ readIORef grBoxRef
    coords@(x, y) <- eventCoordinates
    let grMorph = eBoxGraphMorphism grBox
        graph = M.codomain grMorph
        domGraph = M.domain grMorph
        obj = fetchObj graph coords
        grBox' = case (obj, button, click) of
            (Nothing, LeftButton, DoubleClick) ->
                EditingBox
                    (GM.updateCodomain (newNode coords webColors graph) grMorph)
                    NoMouseAction
            (Just (Node n), RightButton, SingleClick) ->
                grBox { mouseAction = NodeSel (CodNode n) }
            (Just (Node n), LeftButton, SingleClick) ->
                case mouseAction grBox of
                    EdgeCreation (CodNode s) -> 
                        EditingBox
                            (GM.updateCodomain (newEdge s n graph) grMorph)
                            NoMouseAction
                    EdgeCreation (DomNode s) ->
                        let domGraph' = attributeTypeColor s domGraph n graph
                            grMorph' = GM.updateDomain domGraph' grMorph
                        in EditingBox (GM.updateNodes s n grMorph') NoMouseAction
                    otherwise -> grBox {mouseAction = EdgeCreation (CodNode n)}
            otherwise -> grBox
    liftIO $ writeIORef grBoxRef grBox'
    liftIO $ widgetQueueDraw widget
    liftIO $ widgetQueueDraw domWidget
    return True

attributeTypeColor :: G.NodeId -> Graph -> G.NodeId -> Graph -> Graph
attributeTypeColor s sgraph t tgraph =
    let tPayload = G.nodePayload tgraph t
    in case tPayload of
        Just (_, color) ->
            G.updateNodePayload s sgraph (\(coords, _) -> (coords, color))
        otherwise -> sgraph


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
    | norm coords nCoords < defRadius = True
    | otherwise = False
  where
    p = G.nodePayload graph n
    nCoords = fst $ fromJust p
    

normalize :: Double -> Double -> Coords -> Coords
normalize width height coords@(x, y)
    | height == 0 || width == 0 = coords
    | otherwise = (x / width, y / height)


newNode :: Coords -> (G.NodeId -> Kolor) -> Graph -> Graph
newNode coords f graph =
    G.insertNodeWithPayload newId graph (coords, f newId)
  where
    newId = length . G.nodes $ graph

newEdge :: G.NodeId -> G.NodeId -> Graph -> Graph
newEdge src tgt graph =
    G.insertEdge edgeId src tgt graph
  where
    edgeId = length . G.edges $ graph

mouseMove :: WidgetClass widget
          => widget -> IORef EditingBox
          -> EventM EMotion Bool
mouseMove canvas grBoxRef = do
    grBox <- liftIO $ readIORef grBoxRef
    coords@(x, y) <- eventCoordinates
    let typedGraph = eBoxGraphMorphism grBox
        graph = M.domain typedGraph
        codGraph = M.codomain typedGraph
        grMouse = mouseAction grBox
        nR = GM.nodeRelation typedGraph
        eR = GM.edgeRelation typedGraph
    case grMouse of
       (NodeSel (DomNode n)) ->
            let graph' =
                    G.updateNodePayload n graph (\(_, color) -> (coords, color))
            in liftIO $ writeIORef grBoxRef $
                grBox { eBoxGraphMorphism = GM.updateDomain graph' typedGraph }
       (NodeSel (CodNode n)) ->
            let codGraph' =
                    G.updateNodePayload n codGraph (\(_, color) -> (coords, color))
            in liftIO $ writeIORef grBoxRef $
                grBox { eBoxGraphMorphism = GM.updateCodomain codGraph' typedGraph }

       otherwise -> return ()

    liftIO $ widgetQueueDraw canvas
    return True

 
addMainCallbacks :: GUI -> IORef Grammar -> IO ()
addMainCallbacks gui gramRef = do
    let window   = mainWindow gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
    window `on` objectDestroy $ mainQuit
    iGraphButton `on` buttonActivated $ iGraphDialog gramRef
    return ()

updateCanvas :: DrawingArea -> IORef EditingBox
             -> (GM.GraphMorphism NodePayload EdgePayload -> Graph) -> Render ()
updateCanvas canvas grBoxRef f = do
    typedGraph <- liftIO $ readIORef grBoxRef >>= return . eBoxGraphMorphism
    let graph = f typedGraph
    render (RGraph graph)
    
        
renderColor :: Kolor -> Gtk.Render ()
renderColor k = setSourceRGB r g b
  where
    rgb = toSRGB k
    (r, g, b) = (channelRed rgb, channelGreen rgb, channelBlue rgb)

mouseRelease :: IORef EditingBox -> IORef EditingBox -> EventM EButton Bool
mouseRelease grBoxRef tGrBoxRef = do
    liftIO $ do modifyIORef grBoxRef $ cancelDrag
                modifyIORef tGrBoxRef $ cancelDrag
    return True
  where
    cancelDrag box = box { mouseAction = NoMouseAction }

