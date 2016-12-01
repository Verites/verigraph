module Abstract.DPO.Derivation
( Derivation(..)
, generateDerivation
, getDObjects
, getAllBottomObjects
, getLefts
, getRights
)

where

import Abstract.DPO
import Abstract.Morphism

data Derivation m = Derivation
  { production :: Production m
  , match :: m
  , comatch :: m
  , gluing :: m
  , dToG :: m
  , dToH :: m
  } deriving (Eq, Show, Read)

generateDerivationUnsafe :: (DPO m) => m -> Production m -> Derivation m
generateDerivationUnsafe match rule = Derivation rule match n k f g
  where
    (k,n,f,g) = calculateDPO match rule

-- | Given a match @m@ and a production @p@, it returns @Just d@, where @d@ is the corresponding Derivation if @m@ satisfies the rewriting conditions, or @Nothing@.
generateDerivation :: (DPO m) => DPOConfig -> m -> Production m -> Maybe (Derivation m)
generateDerivation conf match rule =
  if satisfiesRewritingConditions conf rule match then
     Just (generateDerivationUnsafe match rule)
  else Nothing

getDObjects :: (DPO m) =>  [Derivation m] -> [Obj m]
getDObjects = fmap (domain . dToG)

getLefts :: [Derivation m] -> [m]
getLefts = fmap dToG

getRights :: [Derivation m] -> [m]
getRights = fmap dToH

getAllBottomObjects :: (DPO m) => Derivation m -> [Obj m]
getAllBottomObjects ds =
  let l = codomain . dToG
      k =   domain . dToG
      r = codomain . dToH
   in [l ds, k ds, r ds]
