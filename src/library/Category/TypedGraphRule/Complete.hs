{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraphRule.Complete (

) where

import Abstract.Category.Complete as C
import Abstract.Category.FinitaryCategory
import Category.TypedGraphRule
import Rewriting.DPO.TypedGraph
import Abstract.Rewriting.DPO
import Category.TypedGraph.CommutingSquares

instance Complete (RuleMorphism a b) where

  calculatePullback = calculatePullback'

-- @
--        g'
--     X──────▶A
--     │       │
--  f' │       │ f
--     ▼       ▼
--     B──────▶C
--        g
-- @
calculatePullback' :: RuleMorphism a b -> RuleMorphism a b -> (RuleMorphism a b, RuleMorphism a b)
calculatePullback' (RuleMorphism fA _ fL fK fR) (RuleMorphism gB _ gL gK gR) = (f',g')
  where
    (f'L, g'L) = C.calculatePullback fL gL
    (f'K, g'K) = C.calculatePullback fK gK
    (f'R, g'R) = C.calculatePullback fR gR

    l = commutingMorphism
          (getLHS gB <&> f'K) f'L
          (getLHS fA <&> g'K) g'L

    r = commutingMorphism
          (getRHS gB <&> f'K) f'R
          (getRHS fA <&> g'K) g'R

    x = buildProduction l r []
    f' = RuleMorphism x gB f'L f'K f'R
    g' = RuleMorphism x fA g'L g'K g'R
