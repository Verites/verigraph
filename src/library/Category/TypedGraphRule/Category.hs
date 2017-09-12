{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraphRule.Category
(  TypedGraphRule
,  RuleMorphism(..)
,  ruleMorphism
,  satisfiesNACRewriting
,  toSndOrderMorphismClass
,  toFstOrderMorphismClass
) where

import           Abstract.Category
import           Abstract.Rewriting.DPO
import           Base.Valid
import           Category.TypedGraph
import           Rewriting.DPO.TypedGraph

-- | A morphism between two first-order rules.
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
    rmDomain         :: TypedGraphRule a b
  , rmCodomain       :: TypedGraphRule a b
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Eq, Show)

ruleMorphism :: TypedGraphRule a b -> TypedGraphRule a b -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> RuleMorphism a b
ruleMorphism = RuleMorphism

instance Valid (RuleMorphism a b) where
    validate (RuleMorphism dom cod mapL mapK mapR) =
      mconcat
        [ withContext "domain" (validate dom)
        , withContext "codomain" (validate cod)
        , withContext "left-hand graph morphism" (validate mapL)
        , withContext "interface graph morphism" (validate mapK)
        , withContext "right-hand graph morphism" (validate mapR)
        , ensure (leftMorphism cod <&> mapK == mapL <&> leftMorphism dom) "Left square doesn't commute"
        , ensure (rightMorphism cod <&> mapK == mapR <&> rightMorphism dom) "Right square doesn't commute"
        ]

satisfiesNACRewriting :: DPO morph => morph -> morph -> Bool
satisfiesNACRewriting l = satisfiesGluingConditions dpoConf prod
  where
    -- Production just to test satisfiesGluingConditions, note that right side is not used.
    prod = Production l undefined []
    dpoConf = MorphismsConfig anyMorphism

instance Category (RuleMorphism a b) where
  type Obj (RuleMorphism a b) = TypedGraphRule a b

  domain = rmDomain
  codomain = rmCodomain
  t2 <&> t1 = RuleMorphism (domain t1)
                (codomain t2)
                (mappingLeft t2 <&> mappingLeft t1)
                (mappingInterface t2 <&> mappingInterface t1)
                (mappingRight t2 <&> mappingRight t1)

  identity t = RuleMorphism t t
            (identity $ leftObject t)
            (identity $ interfaceObject t)
            (identity $ rightObject t)

  newtype MorphismClass (RuleMorphism a b) =
    Cls { toFstOrderMorphismClass :: MorphismClass (TypedGraphMorphism a b) }
  
  anyMorphism = Cls anyMorphism
  monic = Cls monic
  epic = Cls epic
  iso = Cls iso

  f `belongsToClass` Cls c =
    mappingLeft f `belongsToClass` c && mappingInterface f `belongsToClass` c && mappingRight f `belongsToClass` c

  Cls c1 `isSubclassOf` Cls c2 = c1 `isSubclassOf` c2

toSndOrderMorphismClass :: MorphismClass (TypedGraphMorphism a b) -> MorphismClass (RuleMorphism a b)
toSndOrderMorphismClass = Cls