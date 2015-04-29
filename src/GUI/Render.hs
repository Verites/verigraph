{-# LANGUAGE TemplateHaskell #-} -- fclabels

module GUI.Render (
      renderColor
    , render
    , drawCircle
    , nodeRenderType
    , defRadius
    ) where

import Data.Label -- fclabels
--import GUI.Editing (State (..), GraphEditState (..))
import Control.Monad (liftM)
import GUI.Editing
import Graphics.Rendering.Cairo as Gtk
import Graphics.UI.Gtk (Color (..))
import qualified Graph.Graph as G
import qualified Abstract.Relation as R
import Prelude hiding ((.))
import Control.Category


defRadius = 20 :: Double
defLineWidth = 2 :: Double
defBorderColor = Color 65535 65535 65535
defLineColor = Color 0 0 0
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
            Just (coords, renderFunc, checkFunc) -> renderFunc state gstate n
            otherwise -> return ()
      where
        g = _getGraph gstate

instance Renderable REdge where
    render (REdge state gstate n) =
        let gr = get getGraph gstate
            coords = do
                (src, tgt) <- G.nodesConnectedTo gr n
                srcC <- liftM getCoords $ G.nodePayload gr src
                tgtC <- liftM getCoords $ G.nodePayload gr tgt
                return (srcC, tgtC)
        in case coords of
            Just (srcC@(x, y), tgtC@(x', y')) -> do
                let (dirX, dirY) = directionVect srcC tgtC
                    (dx, dy) = (dirX * defRadius, dirY * defRadius)
                    dist = norm srcC tgtC
                setLineWidth defLineWidth
                renderColor defLineColor
                moveTo x y
                lineTo (x' - (2 * dx)) (y' - (2 * dy))
                rotate $ -(angle (dirX, dirY))
                drawHead $ 0.5 * defRadius
                identityMatrix
                stroke
            Nothing -> return ()
        where
            getCoords (c, _, _) = c
            drawHead len = do
                relMoveTo (len / 2) 0
                relLineTo (-len / 2) len
                relLineTo (-len / 2) (-len)



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
    denom = 65535 :: Double
--    rgb = toSRGB k
--    (r, g, b) = (channelRed rgb, channelGreen rgb, channelBlue rgb)


drawCircle :: Color -> GramState -> GraphEditState -> G.NodeId -> Render ()
drawCircle color state gstate n =
    case p of
        Just ((x, y), rF, cF) -> do
            setLineWidth defLineWidth
            renderColor defBorderColor
            Gtk.arc x y defRadius 0 $ 2 * pi
            strokePreserve
            renderColor color
            if sel then fillPreserve >> highlight else fill
            return ()
        otherwise -> return ()
  where
    highlight = do
        setSourceRGBA 0 0 0 0.4
        fill
    p = G.nodePayload (_getGraph gstate) n
    sel = case get selObjects gstate of
            [] -> False
            ns -> (Node n) `elem` ns

nodeRenderType :: GramState -> GraphEditState -> G.NodeId -> Render ()
nodeRenderType state gstate n =
    case newP of
        Just (_, rF, _) -> rF state gstate n
        _ -> return ()
  where
    nodeTypes = R.apply (_getNodeRelation gstate) n
    tGraph = get (getGraph . getTypeGraph) state
    newP = case nodeTypes of
               (x:xs) -> G.nodePayload tGraph x
               _ -> Nothing

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

