module TypedGraph.Morphism.Teste (

)

where

import Data.Set

coEqualizer :: (a -> b) -> Set a -> Set b -> Set (b,x)
coEqualizer _ _ _ = empty
