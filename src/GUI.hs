 module GUI (runGUI) where
-- module GUI (createGUI, addMainCallbacks, showGUI, NodePayload, EdgePayload) where

import Control.Monad.Trans.Class (lift)
import qualified Graph as G
import qualified GraphMorphism as GM
import qualified GraphGrammar as GG
import qualified GraphRule as GR
import Data.Colour.Names
import Data.Colour.SRGB (Colour, toSRGB, RGB (..))
import Data.Colour.Palette.ColorSet (Kolor, webColors, infiniteWebColors)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.IORef
import qualified Data.Tree as T
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
type Rule  = GR.GraphRule NodePayload EdgePayload

data RNode = RNode Graph G.NodeId
data REdge = REdge Graph G.EdgeId
data RGraph = RGraph Graph

data GrammarTree = GTrGraph GraphMorphism | 
                   GTrTGraph Graph |
                   GTrRule (String, Rule) |
                   GTrNode
    deriving Show

{-
instance Show GrammarTree where
    show g = "Test"
-}



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
    addRule :: Button,
    getOkButton :: Button
    }

data EditingBox = EditingBox {
    eBoxGraphMorphism :: GM.TypedGraph NodePayload EdgePayload,
    editingMode :: EditingMode
    }


type MouseAction = Coords -> MouseButton -> Click -> EditingBox -> EditingBox
    

data GUI = GUI {
    treeView :: TreeView,
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
--    gramRef <- newIORef grammar
    gui <- createGUI
    showGUI gui
    addMainCallbacks gui
    return ()
{-
  where
    grammar :: GG.GraphGrammar NodePayload EdgePayload
    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []
-}

createGUI :: IO GUI
createGUI = do
    window <- windowNew
    set window [ windowTitle := "Verigraph" ]
    mainVBox <- vBoxNew False 1
    hBox <- hBoxNew False 1
    vBox0 <- vBoxNew False 1
    vBox1 <- vBoxNew False 1
    containerAdd window mainVBox

    -- Menu Widgets
    menuBar <- menuBarNew
    fileMenuItem <- menuItemNewWithLabel "File"
    fileMenu <- menuNew
    menuShellAppend menuBar fileMenuItem
    menuItemSetSubmenu fileMenuItem fileMenu
    newItem <- menuItemNewWithLabel "New"
    openItem <- menuItemNewWithLabel "Open"
    saveItem <- menuItemNewWithLabel "Save"
    menuAttach fileMenu newItem 0 1 0 1 
    menuAttach fileMenu openItem 0 1 1 2 
    menuAttach fileMenu saveItem 0 1 2 3 
    
    view <- createViewAndModel
    boxPackStart mainVBox menuBar PackNatural 1
    boxPackStart mainVBox hBox PackGrow 1
    boxPackStart hBox vBox0 PackNatural 1
    boxPackStart hBox vBox1 PackNatural 1

    boxPackStart vBox0 view PackGrow 1

    iGraphButton <- buttonNewWithLabel "Edit initial graph"
    addRuleButton <- buttonNewWithLabel "Add rule"
    okButton <- buttonNewWithLabel "OK"
    boxPackStart vBox1 iGraphButton PackNatural 1
    boxPackStart vBox1 addRuleButton PackNatural 1
    boxPackStart vBox1 okButton PackNatural 1
    dummyCanvas <- drawingAreaNew

    let buttons = Buttons iGraphButton addRuleButton okButton
    return $ GUI view window buttons dummyCanvas 

createViewAndModel :: IO TreeView
createViewAndModel = do
--    tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
    tree <- grammarToModel testGrammar
    
{-
    treeStoreInsert tree [] 0 "Graph"
    treeStoreInsert tree [] 1 "TGraph"
    treeStoreInsertTree tree [] 2 ruleTree
-}
--    treeStoreInsert tree [2] 0 ruleTree

    view <- treeViewNew
    col  <- treeViewColumnNew

    treeViewColumnSetTitle col "Graph Grammar"
    treeViewAppendColumn view col
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
--    cellLayoutSetAttributes col renderer tree $ \row -> [ cellText := row ]
    cellLayoutSetAttributes col renderer tree $ \row -> [ cellText := getName row ]

    treeViewSetModel view tree
--    treeViewColumnAddAttribute col renderer "text" 0

    view `on` rowActivated $ rowSelected tree
--    view `on` rowActivated $ editIGraph tree path col
    return view
  where
    grammar :: GG.GraphGrammar NodePayload EdgePayload
    grammar = GG.graphGrammar (GM.empty G.empty G.empty) []
    getName (GTrGraph _) = "Initial Graph"
    getName (GTrTGraph _) = "Type Graph"
    getName GTrNode = "Rules"
    getName (GTrRule _) = "Rule"


--  where
--    rulesNames = map (("rule" ++) . show) rules
--    ruleTree = T.Tree "Rules" ruleNames
--    ruleTree = T.Node "Rules" [T.Node "rule0" [], T.Node "rule1" []]

testGrammar :: Grammar
testGrammar =
    GG.graphGrammar iGraph []
  where
    iGraph = GM.graphMorphism g t nR eR
    g = G.empty :: Graph
    t = G.empty :: Graph
{-
    g = G.insertNodeWithPayload 0 ((100, 100), neutralColor) $
        G.insertNodeWithPayload 1 ((100, 150), neutralColor) $
        G.empty
    t = G.empty
-}
    nR = R.empty [] []
    eR = R.empty [] []

grammarToModel :: Grammar -> IO (TreeStore GrammarTree)
grammarToModel gg = do
    tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
    treeStoreInsert tree [] 0 iGraph
    treeStoreInsert tree [] 1 tGraph
    treeStoreInsertTree tree [] 2 ruleTree
    return tree
  where
    iGraph = GTrGraph $ GG.initialGraph gg
    tGraph = GTrTGraph $ GG.typeGraph gg
    ruleTree = T.Node GTrNode ruleForest
    ruleForest = foldr (\r acc -> (T.Node (GTrRule r) []) : acc) [] rules
    rules  = GG.rules gg



modelToGrammar :: TreeStore GrammarTree -> IO (Maybe Grammar)
modelToGrammar tree = do
    iGraphM <- treeStoreLookup tree [0]
    tGraphM <- treeStoreLookup tree [1]
    rulesM  <- treeStoreLookup tree [2]
    let elems = do iGraphNode <- iGraphM
                   rulesNode  <- rulesM
                   let rules = map T.rootLabel (T.subForest rulesNode)
                   return $ GG.graphGrammar (T.rootLabel iGraphNode)
                                            rules
    return elems

    
    

rowSelected tree path _ = do
    node <- treeStoreLookup tree path
    case node of
        Nothing -> return ()
        Just n -> (editIGraph (T.rootLabel n))

editIGraph :: GrammarTree -> IO ()
editIGraph (GTrGraph gm) = do
    let graph = M.domain gm
        tGraph = M.codomain gm
        nR = GM.nodeRelation gm
        eR = GM.edgeRelation gm
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
--    let gram' = GG.graphGrammar morph (GG.rules gram)
    case response of
        ResponseApply | valid morph -> do
--                           writeIORef gramRef gram'
                           putStrLn $ "morphism valid"
                           widgetDestroy dialog
                      | otherwise -> putStrLn $ "morphism invalid"
        otherwise -> do putStrLn $ "changes to initial graph cancelled"
                        widgetDestroy dialog
    return ()



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
    references@[leftSideRef, middleRef, rightSideRef] <-
        mapM (\_ -> newIORef $ EditingBox emptyGM NoEditing) [1, 1, 1]

    tFrame <- frameNew
    frameSetLabel tFrame "T Graph"
    tCanvas <- drawingAreaNew
    containerAdd tFrame tCanvas
    boxPackEnd contentArea tFrame PackGrow 1 

    mapM_ (\(c, r, f) -> do
                c `on` draw $ updateCanvas r M.domain
                c `on` buttonPressEvent $
                    mouseClick c tCanvas r f
                widgetAddEvents c [Button3MotionMask]
                c `on` motionNotifyEvent $ mouseMove c r) $
          zip3 canvas references [delClick, domClick, domClick]

    tCanvas `on` draw $ updateCanvas middleRef M.codomain
    tCanvas `on` buttonPressEvent $
        mouseClick middleCanvas tCanvas middleRef simpleCodClick
    widgetAddEvents tCanvas [Button3MotionMask]
    tCanvas `on` motionNotifyEvent $ mouseMove tCanvas middleRef

    copyButton <- dialogAddButton dialog "Copy" ResponseOk
    copyButton `on` buttonPressEvent $
                    do
                      liftIO $ do
                          middleEBox <- readIORef middleRef
                          writeIORef leftSideRef middleEBox
                          writeIORef rightSideRef middleEBox
                          widgetQueueDraw leftCanvas
                          widgetQueueDraw rightCanvas
                      return True

    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Apply" ResponseApply
    
    widgetSetSizeRequest dialog 800 600
    widgetShowAll dialog
    response <- dialogRun dialog
    case response of
        ResponseApply -> do
                         leftSideBox <- readIORef leftSideRef
                         let dom = M.domain . eBoxGraphMorphism $ leftSideBox
                         putStrLn $ "dom: " ++ show dom
                         widgetDestroy dialog
        otherwise -> widgetDestroy dialog

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

delClick :: MouseAction
delClick coords@(x, y) button click grBox =
    case (obj, button, click) of
        (Just (Node n), LeftButton, DoubleClick) -> delNode n
        otherwise -> grBox
  where
    grMorph = eBoxGraphMorphism grBox
    dom = M.domain grMorph
    cod = M.codomain grMorph
    obj = fetchObj dom coords
    dom' n = G.removeNode n $
             foldr G.removeEdge dom $ G.incidentEdges dom n
    delNode n =
        EditingBox (GM.updateDomain (dom' n)
                                    grMorph)
                    NoEditing



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
    G.insertNodeWithPayload newId (coords, f newId) graph
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
          => widget -> IORef EditingBox -> EventM EMotion Bool
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

 
addMainCallbacks :: GUI -> IO ()
addMainCallbacks gui = do
    let window   = mainWindow gui
        view = treeView gui
        bs = buttons gui
        iGraphButton = editInitialGraph bs
        addRuleButton = addRule bs
        okButton = getOkButton bs
    window `on` objectDestroy $ mainQuit
    viewRef <- newIORef view
 --   gram <- readIORef gramRef
--    iGraphButton `on` buttonActivated $ iGraphDialog view
--    addRuleButton `on` buttonActivated $ ruleDialog gramRef
--    okButton `on` buttonActivated $ updateModel gram viewRef
    return ()
{-
  where
    updateModel gram viewRef = do
        let iGraph = GG.initialGraph gram
            rules  = GG.rules gram
            ruleTree = foldl (\acc r -> (T.Node (GTrRule r)) : acc) [] rules
        view <- readIORef viewRef
        tree <- treeStoreNew [] :: IO (TreeStore GrammarTree)
        treeStoreInsert tree [] 0 $ GTrGraph iGraph
        treeStoreInsertTree tree [] 0 ruleTree 
        writeIORef viewRef $ treeViewSetModel view tree
-}
        

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
