{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |

module Abstract.Category.PartialMapClassifier where

import           Abstract.Category

class (Category morph) => PartialMapClassifier morph where

  addContextToObject :: Obj morph -> Obj morph
  addContextToObject = codomain @morph . partialMapClassifier

  addContextToMorph :: morph -> morph
  addContextToMorph morphism = totalizePartialMap (partialMapClassifier $ domain morphism) morphism

  partialMapClassifier :: Obj morph -> morph

  totalizePartialMap :: morph -> morph -> morph

  partialInverseOfMono :: morph -> morph
  partialInverseOfMono mono = totalizePartialMap mono (identity $ domain mono)
