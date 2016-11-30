module Abstract.DPO.Derivation
( Derivation(..)
, generateDerivation
, getK
, getObjects
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

getK :: (DPO m) =>  Derivation m -> Obj m
getK = domain . getLHS . production

getObjects :: (DPO m) => Derivation m -> [Obj m]
getObjects ds =
  let l = codomain . getLHS . production
      k =   domain . getLHS . production
      r = codomain . getRHS . production
   in [l ds ,k ds,r ds]
