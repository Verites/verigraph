-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Provides definitions for the Double-Pushout approach to
-- High-Level Rewriting Systems.
module Abstract.Rewriting.DPO
  ( Production
  , buildProduction
  , leftMorphism
  , rightMorphism
  , nacs
  , leftObject
  , rightObject
  , interfaceObject

  , grammar
  , Grammar
  , ObjectFlow (..)
  , RuleSequence
  , NamedProduction
  , getProductionName
  , getProduction
  , start
  , constraints
  , productions
  , findProduction
  , reachableGraphs
  , addReachableGraphs

  -- ** Application

  -- *** Conditions
  -- | In order to apply a production with a particular match, some application
  -- conditions must be satisfied: the gluing condition and the negative
  -- application conditions (NACs). This section provides functions that test
  -- if such conditions are met.

  , DPO(..)
  , satisfiesGluingConditions
  , satisfiesNACs
  , satisfiesRewritingConditions

  -- *** Transformation
  -- | Given a production and a match for its left side, it may be possible
  -- to apply the production and obtain a transformation of the matched graph.
  -- This section provides functions that calculate such transformations.
  , findAllMatches
  , findApplicableMatches
  , calculateDPO
  , calculateComatch
  , rewrite

  -- ** Manipulation
  , invertProductionWithoutNacs
  , nacDownwardShift
  ) where

import Control.Monad

import           Abstract.Category.NewClasses
import           Abstract.Constraint
import           Base.Valid
import           Util.Monad



-- | A Double-Pushout production.
--
-- Consists of two morphisms /'left' : K -> L/ and /'right' : K -> R/,
-- as well as a set of 'nacs' /L -> Ni/.
data Production (cat :: * -> *) morph = Production
  { leftMorphism  :: morph   -- ^ The morphism /K -> L/ of a production
  , rightMorphism :: morph  -- ^ The morphism /K -> R/ of a production
  , nacs  :: [morph] -- ^ The set of nacs /L -> Ni/ of a production
  } deriving (Eq, Show, Read)

leftObject, rightObject, interfaceObject :: (LRNAdhesive cat morph) => Production cat morph -> Obj cat
leftObject = codomain . leftMorphism
rightObject = codomain . rightMorphism
interfaceObject = domain . rightMorphism

instance (LRNAdhesive cat morph, Valid cat morph, Eq (Obj cat)) => Valid cat (Production cat morph) where
  validator (Production l r nacs) = do
    withContext "left morphism" $ do
      validator l
      ensureM (isLeftHandMorphism @cat l) "The morphism is not on the appropriate class"
    withContext "right morphism" $ do
      validator r
      ensureM (isRuleMorphism @cat r) "The morphism is not on the appropriate class"
    ensure (domain @cat l == domain @cat r) "The domains of the LHS and RHS morphisms are not the same"
    forM_ (zip nacs [1..]) $ \(nac, idx :: Int) ->
      withContext ("nac #" ++ show idx) $ do
        validator nac
        ensure (codomain l == domain @cat nac) "The domain is not the LHS object"
      

type NamedProduction cat morph = (String, Production cat morph)

-- | Construct a production from the morphism /l : K -> L/,
-- the morphism /r : K -> R/, and the nacs /L -> Ni/, respectively.
--
-- Note: this doesn't check that the production is valid.
buildProduction :: LRNAdhesive cat morph => morph -> morph -> [morph] -> Production cat morph
buildProduction = Production

data Grammar cat morph = Grammar
  { start           :: Obj cat
  , constraints     :: [Constraint cat morph]
  , productions     :: [NamedProduction cat morph]
  , reachableGraphs :: [(String, Obj cat)]
  }

instance (LRNAdhesive cat morph, Valid cat morph, Valid cat (Obj cat), Eq (Obj cat)) => Valid cat (Grammar cat morph) where
  validator (Grammar s cs rs gs) = do
    withContext "Start graph" $ validator s
    forM_ (zip cs [1..]) $ \(constraint, idx :: Int) ->
      withContext ("Constraint #" ++ show idx) (validator constraint)
    forM_ rs $ \(name, rule) ->
      withContext ("Rule " ++ name) (validator rule)
    forM_ gs $ \(name, graph) ->
      withContext ("Graph " ++ name) (validator graph)
    

-- | Object that uses a Span of Morphisms to connect the right-hand-side of a Production with the left-hand-side of another one
data ObjectFlow cat morph = ObjectFlow 
  { index       :: String -- ^ A identifier for the Object Flow
  , producer    :: String -- ^ The name of the production that will produce the input for the next
  , consumer    :: String -- ^ The name of the production that uses the result of the other
  , spanMapping :: Span cat morph -- ^ A span of Morphisms @Ri <- IO -> Lo@ where @Ri@ is the right-hand-side of the @producer production@ and @Lo@ is the left-hand-side of the @consumer production@
  }

type RuleSequence cat morph = (String,[(String, Production cat morph)],[ObjectFlow cat morph])

grammar :: LRNAdhesive cat morph => Obj cat -> [Constraint cat morph] -> [NamedProduction cat morph] -> Grammar cat morph
grammar s c r = Grammar s c r []

addReachableGraphs :: [(String, Obj cat)] -> Grammar cat morph -> Grammar cat morph
addReachableGraphs gs' (Grammar s c r gs)  = Grammar s c r (gs ++ gs')

getProductionName :: NamedProduction cat morph -> String
getProductionName = fst

getProduction :: NamedProduction cat morph -> Production cat morph
getProduction = snd

findProduction :: String -> Grammar cat morph -> Maybe (Production cat morph)
findProduction name grammar = lookup name (productions grammar)


-- | Class for morphisms whose category is Adhesive-HLR, and which can be
-- used for double-pushout transformations.
class (LRNAdhesive cat morph, FindMorphism cat morph) => DPO cat morph where
  -- | Inverts a production, adjusting the NACs accordingly.
  -- Needs information of nac injective satisfaction (in second-order)
  -- and matches injective.
  invertProduction :: Production cat morph -> Production cat morph

  -- | Given a production /L ←l- K -r→ R/ and a NAC morphism /n : L -> N/, obtain
  -- a set of NACs /n'i : R -> N'i/ that is equivalent to the original NAC.
  shiftNacOverProduction :: Production cat morph -> morph -> cat [morph]

-- | Obtain all matches from the production into the given object, even if they
-- aren't applicable.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findAllMatches :: forall cat morph. DPO cat morph => Production cat morph -> Obj cat -> cat [morph]
findAllMatches production = findMorphisms (matchMorphism @cat) (codomain @cat $ leftMorphism production)

-- | Obtain the matches from the production into the given object that satisfiy the NACs
-- and gluing conditions.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findApplicableMatches :: (DPO cat morph) => Production cat morph -> Obj cat -> cat [morph]
findApplicableMatches production obj =
  filterM (satisfiesRewritingConditions production) =<< findAllMatches production obj



-- | Given a match and a production, calculates the double-pushout diagram
-- for the corresponding transformation.
--
-- Given match \(m : L \to G\) and the production \(L \overset{l}{\leftarrow} K
-- \overset{r}{\to} R\) such that @'satisfiesRewritingConditions' _ _ p m ==
-- True@, returns \(k, n, l'\) and \(r'\) (respectively) such that the following
-- two squares are pushouts.
--
-- @
--       l        r
--    L◀──────K──────▶R
--    │       │       │
--  m │       │ k     │ n
--    ▼       ▼       ▼
--    G◀──────D──────▶H
--         l'    r'
-- @
--
-- The behaviour of this function is undefined when the given match doesn't
-- satisfy the rewriting conditions.
calculateDPO :: DPO cat morph => morph -> Production cat morph -> cat (morph, morph, morph, morph)
calculateDPO m (Production l r _) = do
  (k, l') <- calculatePushoutComplementOfRN l m
  (r', n) <- calculatePushoutAlongRN r k
  return (k, n, l', r')

-- | True if the given match satisfies the gluing condition and NACs of the
-- given production.
satisfiesRewritingConditions :: DPO cat morph => Production cat morph -> morph -> cat Bool
satisfiesRewritingConditions production match =
  satisfiesGluingConditions production match `andM` satisfiesNACs production match

-- | Verifies if the gluing conditions for a production /p/ are satisfied by a match /m/
satisfiesGluingConditions :: DPO cat morph => Production cat morph -> morph -> cat Bool
satisfiesGluingConditions production match = hasPushoutComplementOfRN (leftMorphism production) match

-- | True if the given match satisfies all NACs of the given production.
satisfiesNACs :: DPO cat morph => Production cat morph -> morph -> cat Bool
satisfiesNACs production match = allM (satisfiesSingleNac match) (nacs production)

satisfiesSingleNac :: forall cat morph. DPO cat morph => morph -> morph -> cat Bool
satisfiesSingleNac match nac = null <$> findSpanCommuters (monic @cat) nac match

-- | Given a match and a production, calculate the calculateComatch for the
-- corresponding transformation.
--
-- Given match /m : L -> G/ and the production @p = /L ←l- K -r→ R/@ such that
-- @'satisfiesRewritingConditions' _ _ p m == True@, returns /n/ such that the following two
-- squares are pushouts.
--
-- @
--       l        r
--    L◀──────K──────▶R
--    │       │       │
--  m │       │       │ n
--    ▼       ▼       ▼
--    G◀──────D──────▶H
-- @
--
-- Note: this doesn't test whether the match is for the actual production,
-- nor if the match satisfies all application conditions.
calculateComatch :: DPO cat morph => morph -> Production cat morph -> cat morph
calculateComatch morph prod = do
  (_,m',_,_) <- calculateDPO morph prod
  return m'

-- | Given a match and a production, obtain the rewritten object.
--
-- @rewrite match production@ is equivalent to @'codomain' ('calculateComatch' match production)@
rewrite :: forall cat morph. DPO cat morph => morph -> Production cat morph -> cat (Obj cat)
rewrite morph prod = codomain @cat <$> calculateComatch morph prod

-- | Discards the NACs of a production and inverts it.
invertProductionWithoutNacs :: Production cat morph -> Production cat morph
invertProductionWithoutNacs p = Production (rightMorphism p) (leftMorphism p) []

-- TODO: Is this really a DPO feature?
-- | Given a morphism /m : L -> L'/ and a NAC /n : L -> N/, obtains
-- an equivalent set of NACs /n'i : L' -> N'i/ that is equivalent to the
-- original NAC.
nacDownwardShift :: forall cat morph. (LRNAdhesive cat morph, EM'PairFactorizable cat morph) => morph -> morph -> cat [morph]
nacDownwardShift morph n = map snd <$> findJointlyEpicSquares (monic @cat, n) (matchMorphism @cat, morph)
