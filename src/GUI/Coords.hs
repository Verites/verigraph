module GUI.Coords
    ( mul
    , add
    ) where

type Coords = (Double, Double)

mul :: Double -> Coords -> Coords
infixl 7 `mul`
c `mul` (x', y') = (c * x', c * y')

add :: Coords -> Coords -> Coords
infixl 6 `add`
(x, y) `add` (x', y') = (x + x', y + y')

neg :: Coords -> Coords
neg (x, y) = (-x, -y)

