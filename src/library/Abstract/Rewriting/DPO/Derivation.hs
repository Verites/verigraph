{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Abstract.Rewriting.DPO.Derivation
( Derivation(..)
, generateDerivation
, getDObjects
, getAllBottomObjects
, getLeftBottomMorphisms
, getRightBottomMorphisms
)

where

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO

data Derivation cat morph = Derivation
  { production :: Production cat morph
  , match      :: morph
  , comatch    :: morph
  , gluing     :: morph
  , dToG       :: morph
  , dToH       :: morph
  } deriving (Eq, Show, Read)

generateDerivationUnsafe :: (DPO cat morph) => morph -> Production cat morph -> cat (Derivation cat morph)
generateDerivationUnsafe morph p = do
  (k,n,f,g) <- calculateDPO morph p
  return (Derivation p morph n k f g)

-- | Given a match @m@ and a production @p@, it returns @Just d@, where @d@ is the corresponding Derivation if @m@ satisfies the rewriting conditions, or @Nothing@.
generateDerivation :: DPO cat morph => morph -> Production cat morph -> cat (Maybe (Derivation cat morph))
generateDerivation morph p = do
  canRewrite <- satisfiesRewritingConditions p morph
  if canRewrite then
    Just <$> generateDerivationUnsafe morph p
  else
    return Nothing

getDObjects :: DPO cat morph =>  [Derivation cat morph] -> [Obj cat]
getDObjects = fmap (domain . dToG)

getLeftBottomMorphisms :: [Derivation cat morph] -> [morph]
getLeftBottomMorphisms = fmap dToG

getRightBottomMorphisms :: [Derivation cat morph] -> [morph]
getRightBottomMorphisms = fmap dToH

getBottomObjects :: DPO cat morph => Derivation cat morph -> (Obj cat,Obj cat,Obj cat)
getBottomObjects d =
  let l = codomain . dToG
      k =   domain . dToG
      r = codomain . dToH
   in (l d, k d, r d)

getAllBottomObjects :: DPO cat morph => [Derivation cat morph] -> [Obj cat]
getAllBottomObjects []     = error "can not return objects of an empty derivation"
getAllBottomObjects [d]    = (\(a,b,c) -> [a,b,c]) $ getBottomObjects d
getAllBottomObjects (d:ds) = (\(a,b,_) -> [a,b]) (getBottomObjects d) ++ getAllBottomObjects ds
