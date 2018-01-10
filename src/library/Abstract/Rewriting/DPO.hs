{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Provides definitions for the Double-Pushout approach to
-- High-Level Rewriting Systems.
module Abstract.Rewriting.DPO
  ( Production(..)
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

  , MorphismsConfig(..)

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
  , findJointSurjectiveApplicableMatches
  , calculateDPO
  , calculateComatch
  , rewrite

  -- ** Manipulation
  , invertProductionWithoutNacs
  , nacDownwardShift
  ) where

import qualified Data.List as List

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Abstract.Constraint
import           Base.Valid
import Base.Isomorphic
import qualified Util.List as List



-- | A Double-Pushout production.
--
-- Consists of two morphisms /'left' : K -> L/ and /'right' : K -> R/,
-- as well as a set of 'nacs' /L -> Ni/.
data Production morph = Production {
   leftMorphism  :: morph   -- ^ The morphism /K -> L/ of a production
,  rightMorphism :: morph  -- ^ The morphism /K -> R/ of a production
,  nacs  :: [morph] -- ^ The set of nacs /L -> Ni/ of a production
}  deriving (Eq, Show, Read)

leftObject :: Category morph => Production morph -> Obj morph
leftObject = codomain . leftMorphism

rightObject :: Category morph => Production morph -> Obj morph
rightObject = codomain . rightMorphism

interfaceObject :: Category morph => Production morph -> Obj morph
interfaceObject = domain . leftMorphism

instance (MAdhesive morph, Valid morph, Eq (Obj morph)) => Valid (Production morph) where
  validate (Production l r nacs) =
    mconcat $
      [ withContext "left morphism" (validate l)
      , withContext "right morphism" (validate r)
      , ensure (isInclusion l) "The left side of the production is not monic"
      , ensure (isInclusion r) "The right side of the production is not monic"
      , ensure (domain l == domain r) "The domains of the left and right morphisms aren't the same"
      ] ++ zipWith validateNac nacs ([1..] :: [Int])
    where
      validateNac nac index =
        mconcat
          [ withContext ("NAC #" ++ show index) (validate nac)
          , ensure (codomain l == domain nac) ("The domain of NAC #" ++ show index ++ " is not the left side of the production")
          ]

instance {-# OVERLAPS #-} (Iso (Obj morph), FindMorphism morph, Show morph) => Iso (Production morph) where
  (Production l1 r1 nacs1) ~= (Production l2 r2 nacs2) =
    any isomorphismExtends $ findMorphisms iso (domain l1) (domain l2)
    where
      isomorphismExtends fK = any extendsToNacs leftExtensions && not (List.null rightExtensions)
        where
          rightExtensions = findSpanCommuters iso r1 (r2 <&> fK)
          leftExtensions = findSpanCommuters iso l1 (l2 <&> fK)
      extendsToNacs fL = List.correspondsOneToOne (equivalentNac fL) nacs1 nacs2
      equivalentNac fL n1 n2 = not . List.null $ findSpanCommuters iso n1 (n2 <&> fL)


type NamedProduction morph = (String, Production morph)


data Grammar morph = Grammar {
   start           :: Obj morph
,  constraints     :: [Constraint morph]
,  productions     :: [NamedProduction morph]
,  reachableGraphs :: [(String, Obj morph)]
}

instance (MAdhesive morph, Valid morph, Valid (Obj morph), Eq (Obj morph)) => Valid (Grammar morph) where

  validate (Grammar s c r rg) =
    mconcat $
      [ withContext "Start graph" (validate s)]
      ++ zipWith validateConstraint c ([1..] :: [Int])
      ++ map validateProduction r
      ++ map validateGraph rg
    where
      validateConstraint constraint index =
        mconcat [ withContext ("Constraint #" ++ show index) (validate constraint) ]
      validateProduction (name, production) =
        mconcat [ withContext ("Rule " ++ name) (validate production)]
      validateGraph (name, graph) =
        mconcat [ withContext ("Graph " ++ name) (validate graph)]


-- | Object that uses a Span of Morphisms to connect the right-hand-side of a Production with the left-hand-side of another one
data ObjectFlow morph =
  ObjectFlow {
  index       :: String -- ^ A identifier for the Object Flow
, producer    :: String -- ^ The name of the production that will produce the input for the next
, consumer    :: String -- ^ The name of the production that uses the result of the other
, spanMapping :: Span morph -- ^ A span of Morphisms @Ri <- IO -> Lo@ where @Ri@ is the right-hand-side of the @producer production@ and @Lo@ is the left-hand-side of the @consumer production@
}

type RuleSequence morph = (String,[(String, Production morph)],[ObjectFlow morph])

grammar :: Obj morph -> [Constraint morph] -> [NamedProduction morph] -> Grammar morph
grammar s c r = Grammar s c r []

addReachableGraphs :: [(String, Obj morph)] -> Grammar morph -> Grammar morph
addReachableGraphs gs' (Grammar s c r gs)  = Grammar s c r (gs ++ gs')

getProductionName :: NamedProduction morph -> String
getProductionName = fst

getProduction :: NamedProduction morph -> Production morph
getProduction = snd

findProduction :: String -> Grammar morph -> Maybe (Production morph)
findProduction name grammar = lookup name (productions grammar)


-- | Class for morphisms whose category is Adhesive-HLR, and which can be
-- used for double-pushout transformations.
class (MAdhesive morph, FindMorphism morph) => DPO morph where
  -- | Inverts a production, adjusting the NACs accordingly.
  -- Needs information of nac injective satisfaction (in second-order)
  -- and matches injective.
  invertProduction :: MorphismsConfig morph -> Production morph -> Production morph

  -- | Given a production /L ←l- K -r→ R/ and a NAC morphism /n : L -> N/, obtain
  -- a set of NACs /n'i : R -> N'i/ that is equivalent to the original NAC.
  shiftNacOverProduction :: MorphismsConfig morph -> Production morph -> morph -> [morph]

  -- | Create a special case of jointly epimorphic pairs, where the second morphism is a Nac.
  -- The pairs generated are dependent of the NAC config.
  --
  -- FIXME: rethink this function and the best module to place it
  createJointlyEpimorphicPairsFromNAC :: MorphismsConfig morph -> Obj morph -> morph -> [(morph,morph)]

-- | Obtain all matches from the production into the given object, even if they
-- aren't applicable.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findAllMatches :: (DPO morph) => MorphismsConfig morph -> Production morph -> Obj morph -> [morph]
findAllMatches conf production = findMorphisms (matchRestriction conf) (leftObject production)

-- | Obtain the matches from the production into the given object that satisfiy the NACs
-- and gluing conditions.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findApplicableMatches :: (DPO morph) => MorphismsConfig morph -> Production morph -> Obj morph -> [morph]
findApplicableMatches conf production obj =
  filter (satisfiesRewritingConditions conf production) (findAllMatches conf production obj)


-- | Given rules \(\rho_1\) and \(\rho_2\), respectively, returns all jointly
-- surjective pairs \(m_1 : L_1 \to E, m_2 : L_2 \to E\) of applicable matches.
findJointSurjectiveApplicableMatches :: (DPO morph, E'PairCofinitary morph) => MorphismsConfig morph -> Production morph -> Production morph -> [(morph, morph)]
findJointSurjectiveApplicableMatches conf p1 p2 =
  filter (\(m1, m2) -> satisfiesRewritingConditions conf p1 m1 && satisfiesRewritingConditions conf p2 m2)
  $ findJointSurjections (matchRestriction conf, leftObject p1) (matchRestriction conf, leftObject p2)


-- | Given a match and a production, calculates the double-pushout diagram
-- for the corresponding transformation.
--
-- Given match /m : L -> G/ and the production /L ←l- K -r→ R/ such that
-- @'satisfiesRewritingConditions' _ _ p m == True@, returns /k/, /n/, /f/ and /g/ (respectively)
-- such that the following two squares are pushouts.
--
-- @
--       l        r
--    L◀──────K──────▶R
--    │       │       │
--  m │       │ k     │ n
--    ▼       ▼       ▼
--    G◀──────D──────▶H
--         f     g
-- @
--
-- Note: this doesn't test whether the match is for the actual production,
-- nor if the match satisfies all application conditions.
calculateDPO :: DPO morph => morph -> Production morph -> (morph,morph, morph,morph)
calculateDPO m (Production l r _) =
  let (k, f) = calculatePushoutComplementAlongM l m
      (g, n) = calculatePushoutAlongM r k
  in (k, n, f, g)

-- | True if the given match satisfies the gluing condition and NACs of the
-- given production.
satisfiesRewritingConditions :: DPO morph => MorphismsConfig morph -> Production morph -> morph -> Bool
satisfiesRewritingConditions conf production match =
  satisfiesGluingConditions conf production match && satisfiesNACs conf production match

-- | Verifies if the gluing conditions for a production /p/ are satisfied by a match /m/
satisfiesGluingConditions :: forall morph. DPO morph => MorphismsConfig morph -> Production morph -> morph -> Bool
satisfiesGluingConditions _ production = hasPushoutComplementAlongM (leftMorphism production)

-- | True if the given match satisfies all NACs of the given production.
satisfiesNACs :: DPO morph => MorphismsConfig morph -> Production morph -> morph -> Bool
satisfiesNACs conf production match =
  all (satisfiesSingleNac conf match) (nacs production)

satisfiesSingleNac :: DPO morph => MorphismsConfig morph -> morph -> morph -> Bool
satisfiesSingleNac _ match nac =
  let nacMatches = findMonomorphisms (codomain nac) (codomain match)
      commutes nacMatch = nacMatch <&> nac == match
  in not $ any commutes nacMatches

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
calculateComatch :: DPO morph => morph -> Production morph -> morph
calculateComatch morph prod = let (_,m',_,_) = calculateDPO morph prod in m'

-- | Given a match and a production, obtain the rewritten object.
--
-- @rewrite match production@ is equivalent to @'codomain' ('calculateComatch' match production)@
rewrite :: DPO morph => morph -> Production morph -> Obj morph
rewrite morph prod =
  codomain (calculateComatch morph prod)

-- | Discards the NACs of a production and inverts it.
invertProductionWithoutNacs :: Production morph -> Production morph
invertProductionWithoutNacs p = Production (rightMorphism p) (leftMorphism p) []

data MorphismsConfig morph = MorphismsConfig
  { matchRestriction :: MorphismClass morph }


-- TODO: Is this really a DPO feature?
-- | Given a morphism /m : L -> L'/ and a NAC /n : L -> N/, obtains
-- an equivalent set of NACs /n'i : L' -> N'i/ that is equivalent to the
-- original NAC.
nacDownwardShift :: forall morph. E'PairCofinitary morph => MorphismsConfig morph -> morph -> morph -> [morph]
nacDownwardShift conf morph n = newNacs
  where
    pairs = findJointSurjectionSquares (monic @morph, n) (matchRestriction conf, morph)
    newNacs = map snd pairs
