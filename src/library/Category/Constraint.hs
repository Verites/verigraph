module Category.Constraint
  ( AtomicConstraint (..)
  , buildNamedAtomicConstraint
  , satisfiesAtomicConstraint
  , satisfiesAllAtomicConstraints
  , Constraint (..)
  , satisfiesConstraint
  , satisfiesAllConstraints
  ) where

import           Category.Morphism
import           Abstract.Valid

data AtomicConstraint m = AtomicConstraint {
        name     :: String,
        morphism :: m,
        positive :: Bool
      } deriving (Show)

instance Valid m => Valid (AtomicConstraint m) where
  validate = validate . morphism

buildNamedAtomicConstraint :: String -> m -> Bool -> AtomicConstraint m
buildNamedAtomicConstraint = AtomicConstraint

premise :: (Morphism m) => AtomicConstraint m -> Obj m
premise = domain . morphism

conclusion :: (Morphism m) => AtomicConstraint m -> Obj m
conclusion = codomain . morphism

-- | Given an object @G@ and a AtomicConstraint @a : P -> C@, check whether @G@ satisfies the AtomicConstraint @a@
satisfiesAtomicConstraint :: (FindMorphism m) => Obj m -> AtomicConstraint m -> Bool
satisfiesAtomicConstraint object constraint = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findMonomorphisms (premise constraint) object
    qs = findMonomorphisms (conclusion constraint) object
    a = morphism constraint
    positiveSatisfaction = all (\p ->       any (\q -> compose a q == p) qs) ps
    negativeSatisfaction = all (\p -> not $ any (\q -> compose a q == p) qs) ps
    allPremisesAreSatisfied = if positive constraint then positiveSatisfaction else negativeSatisfaction

-- | Given an object @G@ and a list of AtomicConstraints @a : P -> C@, check whether @G@ satisfies the all them
satisfiesAllAtomicConstraints :: (FindMorphism m) => Obj m -> [AtomicConstraint m] -> Bool
satisfiesAllAtomicConstraints object = all (satisfiesAtomicConstraint object)

data Constraint m =
    Atomic { atomic :: AtomicConstraint m }
  | And { lc :: Constraint m,
          rc :: Constraint m}
  | Or{ lc :: Constraint m,
        rc :: Constraint m}
  | Not { nc :: Constraint m }
  deriving (Show)

instance Valid m => Valid (Constraint m) where
    validate cons = case cons of
      Atomic a -> validate a
      Not b    -> validate (nc b)
      And a b  -> mconcat [validate (lc a), validate (rc b)]
      Or a b   -> mconcat [validate (lc a), validate (rc b)]

-- | Given an object @G@ and a Constraint @c@ (a Boolean formula over atomic constraints), check whether @G@ satisfies @c@
satisfiesConstraint :: (FindMorphism m) => Obj m -> Constraint m -> Bool
satisfiesConstraint object constraint =
  case constraint of
    Atomic atomic -> satisfiesAtomicConstraint object atomic
    Not nc -> not $ satisfiesConstraint object nc
    And lc rc -> satisfiesConstraint object lc && satisfiesConstraint object rc
    Or lc rc -> satisfiesConstraint object lc || satisfiesConstraint object rc

-- | Given an object @G@ and a list of Constraints (Boolean formulas over atomic constraints), check whether @G@ satisfies the all them
satisfiesAllConstraints :: (FindMorphism m) => Obj m -> [Constraint m] -> Bool
satisfiesAllConstraints object = all (satisfiesConstraint object)
