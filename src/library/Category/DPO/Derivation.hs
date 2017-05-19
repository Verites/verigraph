module Category.DPO.Derivation
( Derivation(..)
, generateDerivation
, getDObjects
, getAllBottomObjects
, getLeftBottomMorphisms
, getRightBottomMorphisms
)

where

import           Category.AdhesiveHLR
import           Category.DPO.Core

data Derivation m = Derivation
  { production :: Production m
  , match      :: m
  , comatch    :: m
  , gluing     :: m
  , dToG       :: m
  , dToH       :: m
  } deriving (Eq, Show, Read)

generateDerivationUnsafe :: (DPO m) => m -> Production m -> Derivation m
generateDerivationUnsafe m p = Derivation p m n k f g
  where
    (k,n,f,g) = calculateDPO m p

-- | Given a match @m@ and a production @p@, it returns @Just d@, where @d@ is the corresponding Derivation if @m@ satisfies the rewriting conditions, or @Nothing@.
generateDerivation :: (DPO m) => MorphismsConfig -> m -> Production m -> Maybe (Derivation m)
generateDerivation conf m p =
  if satisfiesRewritingConditions conf p m then
     Just (generateDerivationUnsafe m p)
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
