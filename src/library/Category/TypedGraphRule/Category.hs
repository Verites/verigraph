{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Category.TypedGraphRule.Category
( RuleMorphism(..)
, TGRuleCat
, TGRuleConfig(..)
, MatchRestriction(..)
, TGRuleMorphismClass
, TGraphMorphismClass(..)
, fixedClass
, matchMorphismsClass
, runCat
, liftTGraph
, resolveClass
, asTGraphClass
, belongsTo
) where

import           Control.Monad.Reader
import           Control.Monad.List

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Category.TypedGraph (TGraphCat, TGraphConfig, TypedGraphMorphism, MatchRestriction(..))
import           Category.TypedGraph.Category (TGraphMorphismClass(..))
import qualified Category.TypedGraph.Category as TGraph
import           Rewriting.DPO.TypedGraph
import Util.Monad

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

data RuleMorphism n e =
  RuleMorphism {
    rmDomain         :: TypedGraphRule n e
  , rmCodomain       :: TypedGraphRule n e
  , mappingLeft      :: TypedGraphMorphism n e
  , mappingInterface :: TypedGraphMorphism n e
  , mappingRight     :: TypedGraphMorphism n e
  } deriving (Eq, Show)

newtype TGRuleCat n e a = TGRC { unTGRC :: ReaderT (TGRuleConfig n e) (TGraphCat n e) a }
  deriving (Functor, Applicative, Monad)

data TGRuleConfig n e = TGRuleConfig
  { fstOrderConfig :: TGraphConfig n e
  , matchRestriction :: MatchRestriction
  }

runCat :: TGRuleCat n e a -> TGRuleConfig n e -> a
runCat (TGRC action) config = TGraph.runCat (runReaderT action config) (fstOrderConfig config)

liftTGraph :: TGraphCat n e a -> TGRuleCat n e a
liftTGraph = TGRC . lift

type TGRuleMorphismClass = TGraphMorphismClass

instance Category (TGRuleCat n e) (RuleMorphism n e) where
  type Obj (TGRuleCat n e) = TypedGraphRule n e

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

  data MorphismClass (TGRuleCat n e)
    = FixedClass TGRuleMorphismClass
    | MatchMorphisms

  anyMorphism = FixedClass AllMorphisms
  monic = FixedClass Monomorphisms
  epic = FixedClass Epimorphisms
  iso = FixedClass Isomorphisms

  isSubclassOf c1 c2 = isSubclassOf' <$> resolveClass c1 <*> resolveClass c2
    where
      _ `isSubclassOf'` AllMorphisms = True
      Monomorphisms `isSubclassOf'` Monomorphisms = True
      Epimorphisms `isSubclassOf'` Epimorphisms = True
      Isomorphisms `isSubclassOf'` Isomorphisms = True
      Isomorphisms `isSubclassOf'` Monomorphisms = True
      Isomorphisms `isSubclassOf'` Epimorphisms = True
      _ `isSubclassOf'` _ = False
  
  belongsToClass f c = belongsTo f <$> resolveClass c 

fixedClass :: TGRuleMorphismClass -> MorphismClass (TGRuleCat n e)
fixedClass = FixedClass

matchMorphismsClass :: MorphismClass (TGRuleCat n e)
matchMorphismsClass = MatchMorphisms

resolveClass :: MorphismClass (TGRuleCat n e) -> TGRuleCat n e TGRuleMorphismClass
resolveClass (FixedClass c) = return c
resolveClass MatchMorphisms = do
  restriction <- TGRC $ asks matchRestriction
  return $ case restriction of
    AllMatches -> AllMorphisms
    MonicMatches -> Monomorphisms

asTGraphClass :: MorphismClass (TGRuleCat n e) -> TGRuleCat n e (MorphismClass (TGraphCat n e))
asTGraphClass = fmap TGraph.fixedClass . resolveClass

belongsTo :: RuleMorphism n e -> TGRuleMorphismClass -> Bool
belongsTo _ AllMorphisms = True
belongsTo f c =
  (mappingLeft f `TGraph.belongsTo` c)
  && (mappingInterface f `TGraph.belongsTo` c)
  && (mappingRight f `TGraph.belongsTo` c)


instance MFinitary (TGRuleCat n e) (RuleMorphism n e) where
  subobject = monic
  -- TODO: implement getMorphismToCanonicalSubobject and findSubobjectsOf for RuleMorphism

instance ECofinitary (TGRuleCat n e) (RuleMorphism n e) where
  quotient = epic
  -- TODO: implement getMorphismFromCanonicalQuotient and findQuotientsOf for RuleMorphism

instance EM'Factorizable (TGRuleCat n e) (RuleMorphism n e) where
  monicFactor = monic
  -- TODO: implement factorize for RuleMorphism

instance EM'PairFactorizable (TGRuleCat n e) (RuleMorphism n e) where

  findJointlyEpicPairs (c1', p1) (c2', p2) = do
    c1 <- TGraph.fixedClass <$> resolveClass c1'
    c2 <- TGraph.fixedClass <$> resolveClass c2'
    let findJointlyEpic x y = findJointlyEpicPairs (c1, x) (c2, y)
    liftTGraph . runListT $ do
      {- Find jointly epic K1 --ek1-> KE <-ek2-- K2 -}
      (ek1, ek2) <- pickOne $ findJointlyEpic (interfaceObject p1) (interfaceObject p2)

      {- Find jointly epic L1 --el1-> LE <-el2-- L2 and monomorphism KE --le-> LE making the following commute
                el1      el2     
             L1─────▶LE◀─────L2
             ▲        ▲       ▲  
           l1│      le│     l2│  
             │        │       │  
             K1─────▶KE◀─────K2 
                ek1      ek2       -}
      (el1, el2, le) <- createRuleSide' findJointlyEpic leftMorphism ek1 ek2

      {- Find jointly epic R1 --er1-> RE <-er2-- R2 and monomorphism KE --re-> RE making the following commute
                er1      er2     
             R1─────▶RE◀─────R2
             ▲        ▲       ▲  
           r1│      re│     r2│  
             │        │       │  
             K1─────▶KE◀─────K2 
                ek1      ek2       -}
      (er1, er2, re) <- createRuleSide' findJointlyEpic rightMorphism ek1 ek2

      transposedNacs <- (++) <$> transposeNacs el1 (nacs p1) <*> transposeNacs el2 (nacs p2)
      let rule = buildProduction le re transposedNacs
      return (RuleMorphism p1 rule el1 ek1 er1, RuleMorphism p2 rule el2 ek2 er2)
    where
      transposeNacs l = lift . mapM (fmap snd . calculatePushout l)
    
      -- | Create one side (left or ride) of the jointly epic pair of rule morphisms.
      --
      -- Given the jointly epic pair of graph morphisms K1 --ek1-> KE <-ek2-- K2,
      -- finds the jointly epic S1 --es1-> SE <-es2-- S2 and monomorphism KE --se-> SE making the following commute
      --         es1      es2     
      --      S1─────▶SE◀─────S2
      --      ▲        ▲       ▲  
      --    s1│      se│     s2│  
      --      │        │       │  
      --      K1─────▶KE◀─────K2 
      --         ek1      ek2
      createRuleSide' findJointlyEpic sideMorphism ek1 ek2 = do
        let (s1, s2) = (sideMorphism p1, sideMorphism p2)
        (es1, es2) <- pickOne $ findJointlyEpic (codomain s1) (codomain s2)
        se <- pickOne $ findSpanCommuters monic ek1 (es1 <&> s1)
        guard (es2 <&> s2 == se <&> ek2)
        return (es1, es2, se)
