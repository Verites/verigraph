module Abstract.Rewriting.DPO.Derivation
( Derivation(..)
, generateDerivation
, getDObjects
, getAllBottomObjects
, getLeftBottomMorphisms
, getRightBottomMorphisms
)

where

import           Abstract.Category
import           Abstract.Rewriting.DPO

data Derivation morph = Derivation
  { production :: Production morph
  , match      :: morph
  , comatch    :: morph
  , gluing     :: morph
  , dToG       :: morph
  , dToH       :: morph
  } deriving (Eq, Show, Read)

generateDerivationUnsafe :: (DPO morph) => morph -> Production morph -> Derivation morph
generateDerivationUnsafe morph p = Derivation p morph n k f g
  where
    (k,n,f,g) = calculateDPO morph p

-- | Given a match @m@ and a production @p@, it returns @Just d@, where @d@ is the corresponding Derivation if @m@ satisfies the rewriting conditions, or @Nothing@.
generateDerivation :: (DPO morph) => MorphismsConfig morph -> morph -> Production morph -> Maybe (Derivation morph)
generateDerivation conf morph p =
  if satisfiesRewritingConditions conf p morph then
     Just (generateDerivationUnsafe morph p)
  else Nothing

getDObjects :: (DPO morph) =>  [Derivation morph] -> [Obj morph]
getDObjects = fmap (domain . dToG)

getLeftBottomMorphisms :: [Derivation morph] -> [morph]
getLeftBottomMorphisms = fmap dToG

getRightBottomMorphisms :: [Derivation morph] -> [morph]
getRightBottomMorphisms = fmap dToH

getBottomObjects :: (DPO morph) => Derivation morph -> (Obj morph,Obj morph,Obj morph)
getBottomObjects d =
  let l = codomain . dToG
      k =   domain . dToG
      r = codomain . dToH
   in (l d, k d, r d)

getAllBottomObjects :: (DPO morph) => [Derivation morph] -> [Obj morph]
getAllBottomObjects []     = error "can not return objects of an empty derivation"
getAllBottomObjects [d]    = (\(a,b,c) -> [a,b,c]) $ getBottomObjects d
getAllBottomObjects (d:ds) = (\(a,b,_) -> [a,b]) (getBottomObjects d) ++ getAllBottomObjects ds
