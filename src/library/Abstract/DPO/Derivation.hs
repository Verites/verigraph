module Abstract.DPO.Derivation
( Derivation(..)
, generateDerivation
, getDObjects
, getAllBottomObjects
, getLeftBottomMorphisms
, getRightBottomMorphisms
)

where

import           Abstract.AdhesiveHLR
import           Abstract.DPO.Core

data Derivation m = Derivation
  { production :: Production m
  , match      :: m
  , comatch    :: m
  , gluing     :: m
  , dToG       :: m
  , dToH       :: m
  } deriving (Eq, Show, Read)

generateDerivationUnsafe :: (DPO m) => m -> Production m -> Derivation m
generateDerivationUnsafe match rule = Derivation rule match n k f g
  where
    (k,n,f,g) = calculateDPO match rule

-- | Given a match @m@ and a production @p@, it returns @Just d@, where @d@ is the corresponding Derivation if @m@ satisfies the rewriting conditions, or @Nothing@.
generateDerivation :: (DPO m) => MorphismsConfig -> m -> Production m -> Maybe (Derivation m)
generateDerivation conf match rule =
  if satisfiesRewritingConditions conf rule match then
     Just (generateDerivationUnsafe match rule)
  else Nothing

getDObjects :: (DPO m) =>  [Derivation m] -> [Obj m]
getDObjects = fmap (domain . dToG)

getLeftBottomMorphisms :: [Derivation m] -> [m]
getLeftBottomMorphisms = fmap dToG

getRightBottomMorphisms :: [Derivation m] -> [m]
getRightBottomMorphisms = fmap dToH

getBottomObjects :: (DPO m) => Derivation m -> (Obj m,Obj m,Obj m)
getBottomObjects d =
  let l = codomain . dToG
      k =   domain . dToG
      r = codomain . dToH
   in (l d, k d, r d)

getAllBottomObjects :: (DPO m) => [Derivation m] -> [Obj m]
getAllBottomObjects [] = error "can not return objects of an empty derivation"
getAllBottomObjects [d] = (\(a,b,c) -> [a,b,c]) $ getBottomObjects d
getAllBottomObjects (d:ds) = (\(a,b,_) -> [a,b]) (getBottomObjects d) ++ getAllBottomObjects ds
