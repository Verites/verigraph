 module GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

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
import Data.Foldable (mapM_)
import Data.Traversable (sequenceA, traverse)
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Gtk
import Graphics.UI.Gtk.Gdk.EventM
import qualified Morphism as M
import Prelude hiding (mapM_, any)
import qualified Relation as R
import Valid (valid)

defRadius = 20 :: Double
defLineWidth = 2 :: Double
defBorderColor = black
defSpacing = 1
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

data EditingMode = EdgeCreation QElem | NodeSel QElem | NoEditing
    deriving (Show)

data Buttons = Buttons {
    editInitialGraph :: Button,
    addRule :: Button
    }

data EditingBox = EditingBox {
    eBoxGraphMorphism :: GM.TypedGraph NodePayload EdgePayload,
    editingMode :: EditingMode
    }


type MouseAction = Coords -> MouseButton -> Click -> EditingBox -> EditingBox
    

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

runGUI :: IO ()
runGUI = do
    gramRef <- newIORef grammar
    gui <- createGUI
    showGUI gui
    addMainCallbacks gui gramRef
    return ()
  where
    grammar :: GG.GraphGrammar NodePayload EdgePayload
    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []

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
                (GM.graphMorphism graph tGraph nR eR) NoEditing
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

    canvas `on` buttonPressEvent $ mouseClick canvas tCanvas grBoxRef domClick
    canvas `on` draw $ updateCanvas grBoxRef M.domain
    widgetAddEvents canvas [Button3MotionMask]
    canvas `on` motionNotifyEvent $ mouseMove canvas grBoxRef

    
    tCanvas `on` buttonPressEvent $ mouseClick canvas tCanvas grBoxRef codClick
    tCanvas `on` draw $ updateCanvas grBoxRef M.codomain
    widgetAddEvents tCanvas [Button3MotionMask]
    tCanvas `on` motionNotifyEvent $ mouseMove tCanvas grBoxRef

    boxPackStart contentArea frame PackGrow 1
    boxPackStart contentArea tFrame PackGrow 1
    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Apply" ResponseApply

    widgetSetSizeRequest dialog 800 600
    widgetShowAll dialog
    response <- dialogRun dialog
    morph <- readIORef grBoxRef >>= return . mapEdges . eBoxGraphMorphism
    let gram' = GG.graphGrammar morph (GG.rules gram)
    case response of
        ResponseApply | valid morph -> do
                           writeIORef gramRef gram'
                           putStrLn $ "morphism valid"
                           widgetDestroy dialog
                      | otherwise -> putStrLn $ "morphism invalid"
        otherwise -> do putStrLn $ "changes to initial graph cancelled"
                        widgetDestroy dialog
    return ()

ruleDialog :: IORef Grammar -> IO ()
ruleDialog gramRef = do
    gram <- readIORef gramRef

    dialog <- dialogNew
    contentArea <- dialogGetContentArea dialog >>= return . castToBox
    topBox <- hBoxNew True defSpacing -- no grids available yet in haskell's gtk3
    boxPackStart contentArea topBox PackGrow 1

    frames@[leftFrame, middleFrame, rightFrame] <- mapM (\_ -> frameNew) [1, 1, 1]
    mapM_ (\(f, l) -> frameSetLabel f l) $ zip frames ["L", "K", "R"]

    canvas@[leftCanvas, middleCanvas, rightCanvas] <-
        mapM (\_ -> drawingAreaNew) [1, 1, 1]

    mapM_ (\(f, c) -> containerAdd f c) $
          zip frames canvas

    mapM_ (\f -> boxPackStart topBox f PackGrow 1) frames

    let tGraph = GG.typeGraph gram
        nR = R.empty [] (G.nodes tGraph)
        eR = R.empty [] (G.edges tGraph)
        emptyGM = GM.graphMorphism G.empty tGraph nR eR
    leftSideRef <- newIORef $
        EditingBox emptyGM NoEditing
    rightSideRef <- newIORef $
        EditingBox emptyGM NoEditing


    tFrame <- frameNew
    frameSetLabel tFrame "T Graph"
    tCanvas <- drawingAreaNew
    containerAdd tFrame tCanvas
    boxPackEnd contentArea tFrame PackGrow 1 


    middleCanvas `on` draw $ updateCanvas leftSideRef M.domain
    middleCanvas `on` buttonPressEvent $
        mouseClick middleCanvas tCanvas leftSideRef domClick
    widgetAddEvents middleCanvas [Button3MotionMask]
    middleCanvas `on` motionNotifyEvent $ mouseMove middleCanvas leftSideRef


    tCanvas `on` draw $ updateCanvas leftSideRef M.codomain
    tCanvas `on` buttonPressEvent $
        mouseClick middleCanvas tCanvas leftSideRef simpleCodClick
    widgetAddEvents tCanvas [Button3MotionMask]
    tCanvas `on` motionNotifyEvent $ mouseMove tCanvas leftSideRef
    
    widgetSetSizeRequest dialog 800 600
    widgetShowAll dialog
    dialogRun dialog
    return ()

mouseClick :: WidgetClass widget => widget -> widget
           -> IORef EditingBox -> MouseAction -> EventM EButton Bool
mouseClick domWidget codWidget grBoxRef f = do
    coords <- eventCoordinates
    button <- eventButton
    click <- eventClick
    grBox <- liftIO $ readIORef grBoxRef
    let grBox' = f coords button click grBox
    liftIO $ do writeIORef grBoxRef grBox'
                widgetQueueDraw domWidget
                widgetQueueDraw codWidget
    return True                

domClick :: MouseAction
domClick coords@(x, y) button click grBox =
    case (obj, button, click) of
        (Nothing, LeftButton, DoubleClick) -> addNode
        (Just (Node n), RightButton, SingleClick) -> selDomNode n
        (Just (Node n), LeftButton, SingleClick) ->
            case editingMode grBox of
                EdgeCreation (DomNode s) -> addEdge s n
                EdgeCreation (CodNode t) -> bindType n t
                otherwise -> edgeCreationMode n
        otherwise -> grBox
  where
    grMorph = eBoxGraphMorphism grBox
    dom = M.domain grMorph
    cod = M.codomain grMorph
    obj = fetchObj dom coords
    addNode = EditingBox (GM.updateDomain (newNode coords (\_ -> neutralColor) dom)
                                          grMorph)
                         NoEditing
    selDomNode n = grBox { editingMode = NodeSel (DomNode n) }
    addEdge s n = EditingBox (GM.updateDomain (newEdge s n dom) grMorph)
                             NoEditing
    bindType n t = let dom' = attributeTypeColor n dom t cod
                       grMorph' = GM.updateDomain dom' grMorph
                   in EditingBox (GM.updateNodes n t grMorph') NoEditing
    edgeCreationMode n = grBox {editingMode = EdgeCreation (DomNode n)}
        
codClick :: MouseAction
codClick coords@(x, y) button click grBox =
    case (obj, button, click) of
        (Nothing, LeftButton, DoubleClick) -> addNode
        (Just (Node n), RightButton, SingleClick) -> selCodNode n
        (Just (Node n), LeftButton, SingleClick) ->
            case editingMode grBox of
                EdgeCreation (CodNode s) -> addEdge s n
                EdgeCreation (DomNode s) -> bindType s n
                otherwise -> edgeCreationMode n
        otherwise -> grBox
  where
    grMorph = eBoxGraphMorphism grBox
    cod = M.codomain grMorph
    dom = M.domain grMorph
    obj = fetchObj cod coords
    addNode = EditingBox (GM.updateCodomain (newNode coords webColors cod)
                                            grMorph)
                         NoEditing
    selCodNode n = grBox { editingMode = NodeSel (CodNode n) }
    addEdge s n = EditingBox (GM.updateCodomain (newEdge s n cod) grMorph)
                                NoEditing
    bindType n t =  let dom' = attributeTypeColor n dom t cod
                        grMorph' = GM.updateDomain dom' grMorph
                    in EditingBox (GM.updateNodes n t grMorph') NoEditing
    edgeCreationMode n = grBox {editingMode = EdgeCreation (CodNode n)}

simpleCodClick :: MouseAction
simpleCodClick coords@(x, y) button click grBox =
    case (obj, button, click) of
       (Just (Node n), RightButton, SingleClick) -> selCodNode n
       (Just (Node n), LeftButton, SingleClick) ->
           case editingMode grBox of
               EdgeCreation (DomNode s) -> bindType s n
               otherwise -> edgeCreationMode n
       otherwise -> grBox
  where
    grMorph = eBoxGraphMorphism grBox
    cod = M.codomain grMorph
    dom = M.domain grMorph
    obj = fetchObj cod coords
    selCodNode n = grBox { editingMode = NodeSel (CodNode n) }
    bindType n t =  let dom' = attributeTypeColor n dom t cod
                        grMorph' = GM.updateDomain dom' grMorph
                    in EditingBox (GM.updateNodes n t grMorph') NoEditing
    edgeCreationMode n = grBox {editingMode = EdgeCreation (CodNode n)}



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

mapEdges :: GraphMorphism -> GraphMorphism
mapEdges gm = foldr mapEdge gm $ G.edges . M.domain $ gm

mapEdge :: G.EdgeId -> GraphMorphism -> GraphMorphism
mapEdge e gm
    | null nds || null srcNds || null tgtNds || null eds = gm
    | otherwise = GM.updateEdges e e' gm
  where
    dom = M.domain gm
    cod = M.codomain gm
    nds = G.nodesConnectedTo dom e
    (s, t) = head nds
    (srcNds, tgtNds) = (GM.applyNode gm s, GM.applyNode gm t)
    (s', t') = (head srcNds, head tgtNds)
    eds = L.intersect (G.edgesFromNode cod s') (G.edgesIntoNode cod t')
    e' = head eds
    

mouseMove :: WidgetClass widget
          => widget -> IORef EditingBox
          -> EventM EMotion Bool
mouseMove canvas grBoxRef = do
    grBox <- liftIO $ readIORef grBoxRef
    coords@(x, y) <- eventCoordinates
    let typedGraph = eBoxGraphMorphism grBox
        graph = M.domain typedGraph
        codGraph = M.codomain typedGraph
        grMouse = editingMode grBox
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
    addRuleButton `on` buttonActivated $ ruleDialog gramRef
    return ()

updateCanvas :: IORef EditingBox
             -> (GM.GraphMorphism NodePayload EdgePayload -> Graph) -> Render ()
updateCanvas grBoxRef f = do
    typedGraph <- liftIO $ readIORef grBoxRef >>= return . eBoxGraphMorphism
    let graph = f typedGraph
    render (RGraph graph)

mouseRelease :: IORef EditingBox -> IORef EditingBox -> EventM EButton Bool
mouseRelease grBoxRef tGrBoxRef = do
    liftIO $ do modifyIORef grBoxRef $ cancelDrag
                modifyIORef tGrBoxRef $ cancelDrag
    return True
  where
    cancelDrag box = box { editingMode = NoEditing }
        
renderColor :: Kolor -> Gtk.Render ()
renderColor k = setSourceRGB r g b
  where
    rgb = toSRGB k
    (r, g, b) = (channelRed rgb, channelGreen rgb, channelBlue rgb)
