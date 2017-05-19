{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Description : Basic structures and functions to deal with the DPO rewritting approach.

This module includes the definition of a data types for DPO productions and DPO graph grammars,
alongside their constructors and manipulators.

Also, this module has the functions to deal with Productions applicability, nacs satisfactions
and the DPO rewritting.
-}
module Category.DPO.Core

where

import           Category.AdhesiveHLR
import           Category.FinitaryCategory
import           Abstract.Valid



-- | A Double-Pushout production.
--
-- Consists of two morphisms /'left' : K -> L/ and /'right' : K -> R/,
-- as well as a set of 'nacs' /L -> Ni/.
data Production morph = Production {
   left  :: morph   -- ^ The morphism /K -> L/ of a production
,  right :: morph  -- ^ The morphism /K -> R/ of a production
,  nacs  :: [morph] -- ^ The set of nacs /L -> Ni/ of a production
}  deriving (Eq, Show, Read)

instance (FinitaryCategory morph, Valid morph, Eq (Obj morph)) => Valid (Production morph) where
  validate (Production l r nacs) =
    mconcat $
      [ withContext "left morphism" (validate l)
      , withContext "right morphism" (validate r)
      , ensure (isMonomorphism l) "The left side of the production is not monic"
      , ensure (isMonomorphism r) "The right side of the production is not monic"
      , ensure (domain l == domain r) "The domains of the left and right morphisms aren't the same"
      ] ++ zipWith validateNac nacs ([1..] :: [Int])
    where
      validateNac nac index =
        mconcat
          [ withContext ("NAC #" ++ show index) (validate nac)
          , ensure (codomain l == domain nac) ("The domain of NAC #" ++ show index ++ " is not the left side of the production")
          ]


type NamedProduction morph = (String, Production morph)

-- | Construct a production from the morphism /l : K -> L/,
-- the morphism /r : K -> R/, and the nacs /L -> Ni/, respectively.
--
-- Note: this doesn't check that the production is valid.
buildProduction :: morph -> morph -> [morph] -> Production morph
buildProduction = Production

-- | Returns the morphism /K -> L/ of the given production
getLHS :: Production morph -> morph
getLHS = left

-- | Returns the morphism /K -> R/ of the given production
getRHS :: Production morph -> morph
getRHS = right

-- | Returns the set of nacs /L -> Ni/ of the given production
getNACs :: Production morph -> [morph]
getNACs = nacs

data Grammar morph = Grammar {
   start           :: Obj morph
,  constraints     :: [Constraint morph]
,  productions     :: [NamedProduction morph]
,  reachableGraphs :: [(String, Obj morph)]
}

instance (FinitaryCategory morph, Valid morph, Valid (Obj morph), Eq (Obj morph)) => Valid (Grammar morph) where

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
class (AdhesiveHLR morph, FindMorphism morph) => DPO morph where
  -- | Inverts a production, adjusting the NACs accordingly.
  -- Needs information of nac injective satisfaction (in second order)
  -- and matches injective.
  invertProduction :: MorphismsConfig -> Production morph -> Production morph

  -- | Given a production /L ←l- K -r→ R/ and a NAC morphism /n : L -> N/, obtain
  -- a set of NACs /n'i : R -> N'i/ that is equivalent to the original NAC.
  shiftNacOverProduction :: MorphismsConfig -> Production morph -> morph -> [morph]

-- | Obtain all matches from the production into the given object, even if they
-- aren't applicable.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findAllMatches :: (DPO morph) => MorphismsConfig -> Production morph -> Obj morph -> [morph]
findAllMatches conf production =
  findMorphisms
    (matchRestrictionToMorphismType $ matchRestriction conf)
    (codomain $ left production)

-- | Obtain the matches from the production into the given object that satisfiy the NACs
-- and gluing conditions.
--
-- When given `MonoMatches`, only obtains monomorphic matches.
findApplicableMatches :: (DPO morph) => MorphismsConfig -> Production morph -> Obj morph -> [morph]
findApplicableMatches conf production obj =
  filter (satisfiesRewritingConditions conf production) (findAllMatches conf production obj)



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
calculateDPO :: AdhesiveHLR morph => morph -> Production morph -> (morph,morph, morph,morph)
calculateDPO m (Production l r _) =
  let (k, f) = calculatePushoutComplement m l
      (n, g) = calculatePushout k r
  in (k, n, f, g)

-- | True if the given match satisfies the gluing condition and NACs of the
-- given production.
satisfiesRewritingConditions :: DPO morph => MorphismsConfig -> Production morph -> morph -> Bool
satisfiesRewritingConditions conf production match =
  satisfiesGluingConditions conf production match && satisfiesNACs conf production match

-- | Verifies if the gluing conditions for a production /p/ are satisfied by a match /m/
satisfiesGluingConditions :: DPO morph => MorphismsConfig -> Production morph -> morph -> Bool
satisfiesGluingConditions conf production match =
  hasPushoutComplement (matchIsMono, match) (GenericMorphism, left production)
  where
    matchIsMono =
      matchRestrictionToMorphismType (matchRestriction conf)

-- | True if the given match satisfies all NACs of the given production.
satisfiesNACs :: DPO morph => MorphismsConfig -> Production morph -> morph -> Bool
satisfiesNACs conf production match =
  all (satisfiesSingleNac conf match) (nacs production)

satisfiesSingleNac :: DPO morph => MorphismsConfig -> morph -> morph -> Bool
satisfiesSingleNac conf match nac =
  let nacMatches =
        case nacSatisfaction conf of
          MonomorphicNAC ->
            findMonomorphisms (codomain nac) (codomain match)
          PartiallyMonomorphicNAC ->
            partialInjectiveMatches nac match
      commutes nacMatch =
        compose nac nacMatch == match
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
calculateComatch :: AdhesiveHLR morph => morph -> Production morph -> morph
calculateComatch morph prod = let (_,m',_,_) = calculateDPO morph prod in m'

-- | Given a match and a production, obtain the rewritten object.
--
-- @rewrite match production@ is equivalent to @'codomain' ('calculateComatch' match production)@
rewrite :: AdhesiveHLR morph => morph -> Production morph -> Obj morph
rewrite morph prod =
  codomain (calculateComatch morph prod)

-- | Discards the NACs of a production and inverts it.
invertProductionWithoutNacs :: Production morph -> Production morph
invertProductionWithoutNacs p = Production (right p) (left p) []
