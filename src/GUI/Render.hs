{-# LANGUAGE TemplateHaskell #-} -- fclabels

module GUI.Render (
      renderColor
    , render
    , drawCircle
    , drawSquare
    , nodeRenderType
    , defRadius
    ) where

import Control.Monad (guard)
import Data.AdditiveGroup ((^+^), (^-^))
import Data.AffineSpace ((.-.))
import Data.Cross (cross2)
import Data.Label -- fclabels
import Data.Maybe (fromJust)
import Data.VectorSpace (normalized, (^*))
--import GUI.Editing (State (..), GraphEditState (..))
import GUI.Editing
import Graphics.Rendering.Cairo as Gtk
import Graphics.UI.Gtk (Color (..))
import qualified Graph.Graph as G
import qualified Abstract.Relation as R
import Prelude hiding ((.))
import Control.Category
import Debug.Trace (trace)


defRadius = 20 :: Double
defLineWidth = 2 :: Double
fullChannel = 65535
defBorderColor = Color fullChannel fullChannel fullChannel
defLineColor = Color 0 0 0
defSelColor = Color fullChannel 0 0
ctrlPointColor = Color fullChannel fullChannel 0
defSpacing = 1

{- Data types for rendering -}
data RNode = RNode GramState GraphEditState G.NodeId
data REdge = REdge GramState GraphEditState G.EdgeId
data RGraph = RGraph Graph

class Renderable a where
    render :: a -> Render ()

instance Renderable RNode where
    render (RNode state gstate n) =
        case G.nodePayload g n of
            Just p -> do 
                (get nodeRender p) state gstate n
                drawNodeLabel p
            otherwise -> return ()
      where
        g = _getGraph gstate

instance Renderable REdge where
    render (REdge state gstate e) =
        let gr = get getGraph gstate
            coords = do
                (src, tgt) <- G.nodesConnectedTo gr e
                srcC <- fmap (get nodeCoords) $ G.nodePayload gr src
                tgtC <- fmap (get nodeCoords) $ G.nodePayload gr tgt
                p <- G.edgePayload gr e
                return (srcC, tgtC, get ctrlP1 p, get ctrlP2 p, p)
        in case coords of
            Just (srcC@(x, y), tgtC@(x', y'), ctrlP1, ctrlP2, p) -> do
                let -- Control points coodinates.
                    (ctrlX, ctrlY) = ctrlP1
                    (ctrlX', ctrlY') = ctrlP2
                -- Edge drawing
                setLineWidth defLineWidth
                renderColor defLineColor
                moveTo x y
                lineTo ctrlX ctrlY
                lineTo ctrlX' ctrlY'
                lineTo x' y'
                stroke
                -- control point's drawing
                if ctrlPntSelected 0 
                    then renderColor defSelColor
                    else renderColor ctrlPointColor
                drawCtrlPoint ctrlX ctrlY 
                fill
                if ctrlPntSelected 1
                    then renderColor defSelColor
                    else renderColor ctrlPointColor
                drawCtrlPoint ctrlX' ctrlY'
                fill

                -- Edge head drawing
                setLineWidth defLineWidth
                renderColor defLineColor

                moveTo ctrlX' ctrlY'
--                let (dx, dy) = directionVect ctrlP2 tgtC
                -- move to node borders
--                relMoveTo (- defRadius * dx) (- defRadius * dy)
                rotate $ 
                    angle . normalized $ tgtC ^-^ ctrlP2
{-
                rotate $
                    let refP =
                            bezierPoints 0.70 srcC tgtC ctrlP1 ctrlP2
                        deriv = directionVect refP tgtC
                    in angle deriv
-}
                drawHead $ defRadius * 1.5
                fill
                identityMatrix
                if sel p
                    then
                       do --let (cx, cy) = edgeCenter srcC tgtC ctrlP1 ctrlP2
                          --drawCtrlPoint cx cy
                          fill
                    else return ()
                identityMatrix
            otherwise -> return ()
        where
            selected = get selObjects gstate
            ctrlPntSelected id = any (\o -> case o of
                                        Edge e' p ls -> e == e' && id `elem` ls
                                        _ -> False)
                                     selected
            drawHead len = do
                relLineTo (-len / 2) (len / 4)
                relLineTo 0 (- len / 2)
                relLineTo (len / 2) (len / 4)
            drawCtrlPoint x y = do
                setLineWidth defLineWidth
--                renderColor ctrlPointColor
                arc x y (defRadius / 4) 0 $ 2 * pi
            sel p = Edge e p [] `elem` get selObjects gstate
            highlight = setSourceRGBA 0 0 0 0.4


instance Renderable GramState where
    render state =
        case currentGraphState state of
            Just gstate -> do
                let gr = get getGraph gstate
                mapM_ (render . REdge state gstate) $ G.edges gr
                mapM_ (render . RNode state gstate) $ G.nodes gr
            Nothing -> return ()

renderColor :: Color -> Gtk.Render ()
renderColor (Color r g b) = setSourceRGB  r' g' b'
  where
    (r', g', b') = (fromIntegral r / denom,
                    fromIntegral g / denom,
                    fromIntegral b / denom) :: (Double, Double, Double)
    denom = fromIntegral fullChannel
--    rgb = toSRGB k
--    (r, g, b) = (channelRed rgb, channelGreen rgb, channelBlue rgb)


drawCircle :: Color -> GramState -> GraphEditState -> G.NodeId -> Render ()
drawCircle color state gstate n =
    case p of
        Just p -> do
            let (x, y) = get nodeCoords p
            setLineWidth defLineWidth
            renderColor defBorderColor
            Gtk.arc x y defRadius 0 $ 2 * pi
            strokePreserve
            renderColor color
            if sel then fillPreserve >> highlight >> fill else fill
            return ()
        otherwise -> return ()
  where
    highlight = setSourceRGBA 0 0 0 0.4
    p = G.nodePayload (_getGraph gstate) n
    sel = Node n (fromJust p) `elem` get selObjects gstate

drawSquare :: Color -> GramState -> GraphEditState -> G.NodeId -> Render ()
drawSquare color state gstate n =
    case p of
        Just p -> do
            let (x, y) = get nodeCoords p
            setLineWidth defLineWidth
            renderColor defBorderColor
            moveTo (x - defRadius) (y - defRadius)
            relLineTo (2 * defRadius) 0
            relLineTo 0 (2 * defRadius)
            relLineTo (-2 * defRadius) 0
            relLineTo 0 (-2 * defRadius)
            renderColor color
            if sel then fillPreserve >> highlight >> fill else fill
            return ()
        otherwise -> return ()
  where
    highlight = setSourceRGBA 0 0 0 0.4
    p = G.nodePayload (_getGraph gstate) n
    sel = Node n (fromJust p) `elem` get selObjects gstate


drawNodeLabel :: NodePayload -> Render ()
drawNodeLabel p = do
    moveTo (x - 4) (y + 4) -- so it gets centered
    relMoveTo offset 0
    setSourceRGB 0 0 0
    setFontSize 13
    showText label
    fill
  where
    (x, y) = get nodeCoords p
    label = get nodeLabel p
    offset = fromIntegral $ - (length label `div` 2) * 4
   

nodeRenderType :: GramState -> GraphEditState -> G.NodeId -> Render ()
nodeRenderType state gstate n =
    case newP of
        Just p -> (get nodeRender p) state gstate n
        _ -> return ()
  where
    nodeTypes = R.apply (_getNodeRelation gstate) n
    tGraph = get (getGraph . getTypeGraph) state
    newP = case nodeTypes of
               (x:xs) -> G.nodePayload tGraph x
               _ -> Nothing

angle :: Coords -> Double
angle (dx, dy)
    | dx == 0 && dy >= 0 = pi / 2
    | dx == 0 && dy < 0  = (-pi) / 2
    | dx >= 0 = ang
    | dx < 0 = ang - pi
  where
    ang = atan $ dy / dx

