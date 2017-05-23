{-# LANGUAGE TypeFamilies #-}

module SndOrder.Morphism.Core (
  RuleMorphism(..)
, ruleMorphism
, satisfiesNACRewriting
)
where

import           Abstract.Category.FinitaryCategory
import           Abstract.Rewriting.DPO
import           Abstract.Valid
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraph           ()


-- | A morphism between two first order rules.
--
-- The following diagram illustrates such a morphism, omiting the NACs.
--
-- @
--           l1      r1
--       L1◀─────K1─────▶R1
--       │       │       │
--  mapL │   mapK│   mapR│
--       ▼       ▼       ▼
--       L2◀─────K2─────▶R2
--           l2      r2
-- @
--
-- domain = (l1,r1)
--
-- codomain = (l2,r2)
--
-- mappingLeft = mapL
--
-- mappingInterface = mapK
--
-- mappingRight = mapR

data RuleMorphism a b =
  RuleMorphism {
    rmDomain         :: Production (TypedGraphMorphism a b)
  , rmCodomain       :: Production (TypedGraphMorphism a b)
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Eq, Show)

ruleMorphism :: Production (TypedGraphMorphism a b)
             -> Production (TypedGraphMorphism a b)
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> RuleMorphism a b
ruleMorphism = RuleMorphism

instance Valid (RuleMorphism a b) where
    validate (RuleMorphism dom cod mapL mapK mapR) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , withContext "left-hand graph morphism" (validate mapL)
        , withContext "interface graph morphism" (validate mapK)
        , withContext "right-hand graph morphism" (validate mapR)
        , ensure (getLHS cod <&> mapK == mapL <&> getLHS dom) "Left square doesn't commute"
        , ensure (getRHS cod <&> mapK == mapR <&> getRHS dom) "Right square doesn't commute"
        ]

satisfiesNACRewriting :: DPO morph => morph -> morph -> Bool
satisfiesNACRewriting l = satisfiesGluingConditions dpoConf prod
  where
    -- Production just to test satisfiesGluingConditions, note that right side is not used.
    prod = buildProduction l undefined []
    dpoConf = MorphismsConfig AnyMatches undefined
