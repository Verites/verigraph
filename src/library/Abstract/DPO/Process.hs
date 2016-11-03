module Abstract.DPO.Process (
  Process(..)
, firstAtempt
                            )

where

import Abstract.Cocomplete
import Abstract.DPO
import Abstract.DPO.Derivation
import Abstract.Morphism

data Process m = Process
  { productions :: [Production m]
  , coreObject :: Obj m
  }

firstAtempt :: (DPO m) => Derivation m -> Process m
firstAtempt derivation =
  let d1 = dToG derivation
      d2 = dToH derivation
      (d1', d2') = calculatePushout d1 d2
      core = codomain d1'
      oldL = getLHS $ production derivation
      oldR = getRHS $ production derivation
      newL = compose d1' (compose oldL (match derivation))
      newR = compose d2' (compose oldR (comatch derivation))
      newProduction = buildProduction newL newR []
  in Process [newProduction] core


