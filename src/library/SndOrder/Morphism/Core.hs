{-# LANGUAGE TypeFamilies #-}

module SndOrder.Morphism.Core where

import           Abstract.DPO
import           Abstract.Morphism
import           Abstract.Valid
import           TypedGraph.DPO.GraphRule ()
import           TypedGraph.Morphism

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
--
-- TODO: Make polymorphic on the type of morphism?
data RuleMorphism a b =
  RuleMorphism {
    rmDomain         :: Production (TypedGraphMorphism a b)
  , rmCodomain       :: Production (TypedGraphMorphism a b)
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Eq, Show, Read)

ruleMorphism :: Production (TypedGraphMorphism a b)
             -> Production (TypedGraphMorphism a b)
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> RuleMorphism a b
ruleMorphism = RuleMorphism

instance Morphism (RuleMorphism a b) where
    type Obj (RuleMorphism a b) = Production (TypedGraphMorphism a b)

    domain = rmDomain
    codomain = rmCodomain

    compose t1 t2 =
        RuleMorphism (domain t1)
                     (codomain t2)
                     (compose (mappingLeft t1) (mappingLeft t2))
                     (compose (mappingInterface t1) (mappingInterface t2))
                     (compose (mappingRight t1) (mappingRight t2))

    id t = RuleMorphism t t
             (idMap (codomain (getLHS t)) (codomain (getLHS t)))
             (idMap (domain (getLHS t)) (domain (getLHS t)))
             (idMap (codomain (getRHS t)) (codomain (getRHS t)))

    isMonomorphism rm =
      isMonomorphism (mappingLeft rm) &&
      isMonomorphism (mappingInterface rm) &&
      isMonomorphism (mappingRight rm)

    isEpimorphism rm =
      isEpimorphism (mappingLeft rm) &&
      isEpimorphism (mappingInterface rm) &&
      isEpimorphism (mappingRight rm)

    isIsomorphism (RuleMorphism dom cod mapL mapK mapR) =
      isIsomorphism mapL &&
      isIsomorphism mapK &&
      isIsomorphism mapR &&
      compose (getLHS dom) mapL == compose mapK (getLHS cod) &&
      compose (getRHS dom) mapR == compose mapK (getRHS cod)

instance Valid (RuleMorphism a b) where
    validate (RuleMorphism dom cod mapL mapK mapR) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , withContext "left-hand graph morphism" (validate mapL)
        , withContext "interface graph morphism" (validate mapK)
        , withContext "right-hand graph morphism" (validate mapR)
        , ensure (compose mapK (getLHS cod) == compose (getLHS dom) mapL) "Left square doesn't commute"
        , ensure (compose mapK (getRHS cod) == compose (getRHS dom) mapR) "Right square doesn't commute"
        ]

satisfiesNACRewriting :: DPO m => m -> m -> Bool
satisfiesNACRewriting l = satisfiesGluingConditions dpoConf prod
  where
    -- Production just to test satisfiesGluingConditions, note that right side is not used.
    prod = buildProduction l l []
    dpoConf = MorphismsConfig AnyMatches MonomorphicNAC
