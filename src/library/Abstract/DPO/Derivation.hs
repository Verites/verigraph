module Abstract.DPO.Derivation
( Derivation(..)
, generateDerivation
, getKObjects
, getAllObjects
, getLHSs
, getRHSs
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

getKObjects :: (DPO m) =>  [Derivation m] -> [Obj m]
getKObjects = fmap (domain . getLHS . production)



getLHSs :: [Derivation m] -> [m]
getLHSs = fmap (getLHS . production)

--teste :: (DPO m) => [Derivation m] -> [m]
--teste = map (\d -> compose ((getLHS . production) d) (match d))

getRHSs :: [Derivation m] -> [m]
getRHSs = fmap (getRHS . production)

getAllObjects :: (DPO m) => Derivation m -> [Obj m]
getAllObjects ds =
  let l = codomain . getLHS . production
      k =   domain . getLHS . production
      r = codomain . getRHS . production
   in [l ds, k ds, r ds]

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
