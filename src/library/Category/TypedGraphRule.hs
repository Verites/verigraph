{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraphRule
(  RuleMorphism(..)
,  ruleMorphism
,  satisfiesNACRewriting
) where

import           Abstract.Category.FinitaryCategory
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraph                ()
import           Data.TypedGraph.Morphism

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

instance FinitaryCategory (RuleMorphism a b) where
    type Obj (RuleMorphism a b) = Production (TypedGraphMorphism a b)

    domain = rmDomain
    codomain = rmCodomain
    t2 <&> t1 = RuleMorphism (domain t1)
                 (codomain t2)
                 (mappingLeft t2 <&> mappingLeft t1)
                 (mappingInterface t2 <&> mappingInterface t1)
                 (mappingRight t2 <&> mappingRight t1)

    identity t = RuleMorphism t t
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
      mapL <&> getLHS dom == getLHS cod <&> mapK &&
      mapR <&> getRHS dom == getRHS cod <&> mapK
