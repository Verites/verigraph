{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraphRule.Finitary where

import           Abstract.Category.Finitary
import           Category.TypedGraph                   ()
import           Category.TypedGraphRule.Category        


instance MFinitary (RuleMorphism n e) where
  inclusion = toSndOrderMorphismClass inclusion


instance ECofinitary (RuleMorphism n e) where
  surjection = toSndOrderMorphismClass surjection
  

-- E'PairCofinitary is implemented with the Adhesive type classes, because it
-- depends on pushouts along M-morphisms to transfer NACs
