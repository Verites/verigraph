module Abstract.Category.AdhesiveHLR.Constraint
  ( AtomicConstraint
  , buildNamedAtomicConstraint
  , Constraint (..)
  , satisfiesConstraint
  , satisfiesAllConstraints
  ) where

import           Abstract.Category.AdhesiveHLR.Core
import           Abstract.Category.FinitaryCategory
import           Base.Valid

data AtomicConstraint morph = AtomicConstraint {
        name     :: String,
        morphism :: morph,
        positive :: Bool
      } deriving (Show)

instance Valid morph => Valid (AtomicConstraint morph) where
  validate ac = mconcat
    [withContext ("atomic constraint " ++ name ac) (validate $ morphism ac)]

{- | Given a String @name@, a morphism @m@ and a boolean @p@ it returns a
    named atomic constraint which is positive if @p@ is True and negative otherwise -}
buildNamedAtomicConstraint :: String -> morph -> Bool -> AtomicConstraint morph
buildNamedAtomicConstraint = AtomicConstraint

{- | Given an atomic constraint @a : P -> C@, it returns its premise object @P@ -}
premise :: (FinitaryCategory morph) => AtomicConstraint morph -> Obj morph
premise = domain . morphism

{- | Given an atomic constraint @a : P -> C@, it returns its conclusion object @C@ -}
conclusion :: (FinitaryCategory morph) => AtomicConstraint morph -> Obj morph
conclusion = codomain . morphism

-- | Given an object @G@ and a AtomicConstraint @a : P -> C@, it checks whether @G@ satisfies the AtomicConstraint @a@
satisfiesAtomicConstraint :: (AdhesiveHLR morph) => Obj morph -> AtomicConstraint morph -> Bool
satisfiesAtomicConstraint object constraint = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findMMorphisms (premise constraint) object
    qs = findMMorphisms (conclusion constraint) object
    a = morphism constraint
    positiveSatisfaction = all (\p ->       any (\q -> q <&> a == p) qs) ps
    negativeSatisfaction = all (\p -> not $ any (\q -> q <&> a == p) qs) ps
    allPremisesAreSatisfied = if positive constraint then positiveSatisfaction else negativeSatisfaction

{- | A Constraint is a boolean formula over 'AtomicConstraint's -}
data Constraint morph =
    Atomic { atomic :: AtomicConstraint morph }
  | And { lc :: Constraint morph,
          rc :: Constraint morph}
  | Or{ lc :: Constraint morph,
        rc :: Constraint morph}
  | Not { nc :: Constraint morph }
  deriving (Show)

instance Valid morph => Valid (Constraint morph) where
    validate cons = case cons of
      Atomic a -> validate a
      Not b    -> validate (nc b)
      And a b  -> mconcat [validate (lc a), validate (rc b)]
      Or a b   -> mconcat [validate (lc a), validate (rc b)]

-- | Given an object @G@ and a Constraint @c@ (a Boolean formula over atomic constraints), check whether @G@ satisfies @c@
satisfiesConstraint :: (AdhesiveHLR morph) => Obj morph -> Constraint morph -> Bool
satisfiesConstraint object constraint =
  case constraint of
    Atomic atomic -> satisfiesAtomicConstraint object atomic
    Not nc -> not $ satisfiesConstraint object nc
    And lc rc -> satisfiesConstraint object lc && satisfiesConstraint object rc
    Or lc rc -> satisfiesConstraint object lc || satisfiesConstraint object rc

-- | Given an object @G@ and a list of Constraints (Boolean formulas over atomic constraints), check whether @G@ satisfies the all them
satisfiesAllConstraints :: (AdhesiveHLR morph) => Obj morph -> [Constraint morph] -> Bool
satisfiesAllConstraints object = all (satisfiesConstraint object)
