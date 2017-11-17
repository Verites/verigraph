module Abstract.Constraint
  ( AtomicConstraint (..)
  , buildNamedAtomicConstraint
  , satisfiesAtomicConstraint
  , satisfiesAllAtomicConstraints
  , Constraint (..)
  , satisfiesConstraint
  , satisfiesAllConstraints
  ) where

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Base.Valid

data AtomicConstraint morph = AtomicConstraint {
        name     :: String,
        morphism :: morph,
        positive :: Bool
      } deriving (Show)

instance Valid morph => Valid (AtomicConstraint morph) where
  validate = validate . morphism

buildNamedAtomicConstraint :: String -> morph -> Bool -> AtomicConstraint morph
buildNamedAtomicConstraint = AtomicConstraint

premise :: (Category morph) => AtomicConstraint morph -> Obj morph
premise = domain . morphism

conclusion :: (Category morph) => AtomicConstraint morph -> Obj morph
conclusion = codomain . morphism

-- | Given an object @G@ and a AtomicConstraint @a : P -> C@, check whether @G@ satisfies the AtomicConstraint @a@
satisfiesAtomicConstraint :: (FindMorphism morph) => Obj morph -> AtomicConstraint morph -> Bool
satisfiesAtomicConstraint object constraint = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findMonomorphisms (premise constraint) object
    qs = findMonomorphisms (conclusion constraint) object
    a = morphism constraint
    positiveSatisfaction = all (\p ->       any (\q -> q <&> a == p) qs) ps
    negativeSatisfaction = all (\p -> not $ any (\q -> q <&> a == p) qs) ps
    allPremisesAreSatisfied = if positive constraint then positiveSatisfaction else negativeSatisfaction

-- | Given an object @G@ and a list of AtomicConstraints @a : P -> C@, check whether @G@ satisfies the all them
satisfiesAllAtomicConstraints :: (FindMorphism morph) => Obj morph -> [AtomicConstraint morph] -> Bool
satisfiesAllAtomicConstraints object = all (satisfiesAtomicConstraint object)

data Constraint morph =
    Atomic (AtomicConstraint morph)
  | And (Constraint morph) (Constraint morph)
  | Or  (Constraint morph) (Constraint morph)
  | Not (Constraint morph)
  deriving (Show)

instance Valid morph => Valid (Constraint morph) where
    validate cons = case cons of
      Atomic a -> validate a
      Not b    -> validate b
      And a b  -> mconcat [validate a, validate b]
      Or a b   -> mconcat [validate a, validate b]

-- | Given an object @G@ and a Constraint @c@ (a Boolean formula over atomic constraints), check whether @G@ satisfies @c@
satisfiesConstraint :: (FindMorphism morph) => Obj morph -> Constraint morph -> Bool
satisfiesConstraint object constraint =
  case constraint of
    Atomic atomic -> satisfiesAtomicConstraint object atomic
    Not nc -> not $ satisfiesConstraint object nc
    And lc rc -> satisfiesConstraint object lc && satisfiesConstraint object rc
    Or lc rc -> satisfiesConstraint object lc || satisfiesConstraint object rc

-- | Given an object @G@ and a list of Constraints (Boolean formulas over atomic constraints), check whether @G@ satisfies the all them
satisfiesAllConstraints :: (FindMorphism morph) => Obj morph -> [Constraint morph] -> Bool
satisfiesAllConstraints object = all (satisfiesConstraint object)
