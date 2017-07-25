{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
module Abstract.Constraint
  ( AtomicConstraint (..)
  , buildNamedAtomicConstraint
  , satisfiesAtomicConstraint
  , satisfiesAllAtomicConstraints
  , Constraint (..)
  , satisfiesConstraint
  , satisfiesAllConstraints
  ) where

import           Abstract.Category.NewClasses
import           Base.Valid
import           Util.Monad

data AtomicConstraint (cat :: * -> *) morph = AtomicConstraint
  { name     :: String
  , morphism :: morph
  , positive :: Bool
  } deriving (Show)

instance Valid cat morph => Valid cat (AtomicConstraint cat morph) where
  validator = validator . morphism

buildNamedAtomicConstraint :: Category cat morph => String -> morph -> Bool -> AtomicConstraint cat morph
buildNamedAtomicConstraint = AtomicConstraint

premise :: forall cat morph. Category cat morph => AtomicConstraint cat morph -> Obj cat
premise = domain @cat . morphism

conclusion :: forall cat morph. Category cat morph => AtomicConstraint cat morph -> Obj cat
conclusion = codomain @cat . morphism

-- | Given an object @G@ and a AtomicConstraint @a : P -> C@, check whether @G@ satisfies the AtomicConstraint @a@
satisfiesAtomicConstraint :: forall cat morph. FindMorphism cat morph => Obj cat -> AtomicConstraint cat morph -> cat Bool
satisfiesAtomicConstraint object constraint = do
  premiseMatches <- findMorphisms (monic @cat) (premise @cat constraint) object
  allM matchSatisfiesConstraint premiseMatches
  where
    matchSatisfiesConstraint premiseMatch = do
      conclusionMatches <- findSpanCommuters (monic @cat) (morphism constraint) premiseMatch
      return $
        if positive constraint then
          not (null conclusionMatches)
        else
          null conclusionMatches

-- | Given an object @G@ and a list of AtomicConstraints @a : P -> C@, check whether @G@ satisfies the all them
satisfiesAllAtomicConstraints :: forall cat morph. FindMorphism cat morph => Obj cat -> [AtomicConstraint cat morph] -> cat Bool
satisfiesAllAtomicConstraints object = allM (satisfiesAtomicConstraint object)

data Constraint cat morph =
    Atomic (AtomicConstraint cat morph)
  | And (Constraint cat morph) (Constraint cat morph)
  | Or  (Constraint cat morph) (Constraint cat morph)
  | Not (Constraint cat morph)
  deriving (Show)

instance Valid cat morph => Valid cat (Constraint cat morph) where
  validator cons = case cons of
    Atomic a  -> validator a
    Not c     -> validator c
    And c1 c2 -> validator c1 >> validator c2
    Or c1 c2  -> validator c1 >> validator c2

constraintSatisfied :: Monad cat => (AtomicConstraint cat morph -> cat Bool) -> Constraint cat morph -> cat Bool
constraintSatisfied checkAtomic = check
  where
    check (Atomic a) = checkAtomic a
    check (Not c) = not <$> check c
    check (And c1 c2) = check c1 `andM` check c2
    check (Or c1 c2) = check c1 `orM` check c2

-- | Given an object @G@ and a Constraint @c@ (a Boolean formula over atomic constraints), check whether @G@ satisfies @c@
satisfiesConstraint :: FindMorphism cat morph => Obj cat -> Constraint cat morph -> cat Bool
satisfiesConstraint object = constraintSatisfied (satisfiesAtomicConstraint object)

-- | Given an object @G@ and a list of Constraints (Boolean formulas over atomic constraints), check whether @G@ satisfies the all them
satisfiesAllConstraints :: FindMorphism cat morph => Obj cat -> [Constraint cat morph] -> cat Bool
satisfiesAllConstraints object = allM (satisfiesConstraint object)

