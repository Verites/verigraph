{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This Type Class implements partial map classifiers following
--   the construction of a stable system of monos, as described on
--   AGREE -- Algebraic Graph Rewriting with Controlled Embedding (Long Version)
--   available at https://arxiv.org/abs/1411.4597v2
--
--   Category C has an M-Partial Map Classifier if the following diagram are satisfied on a stable system of monos M of C:
--
--          f
--     X ──────▶ Y
--     V         V
--   m │   PB    │ ny = partialMapClassifier
--     ▼         ▼
--     Z ──────▶ T(Y)
--       p(m,f) = totalizePartialMap


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
