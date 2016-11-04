module Abstract.DPO.Process (
  Process(..)
                            )

where

import Abstract.DPO
import Abstract.Morphism

data Process m = Process
  { productions :: [Production m]
  , coreObject :: Obj m
  }
