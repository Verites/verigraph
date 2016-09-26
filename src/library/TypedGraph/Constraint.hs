module TypedGraph.Constraint
( Constraint
, buildNamedConstraint
, satisfiesAllConstraints
, satisfiesConstraint
) where

import           Abstract.Morphism
import           TypedGraph.Graph
import           TypedGraph.Morphism

data Constraint a b = Constraint {
      name     :: String,
      morphism :: TypedGraphMorphism a b,
      positive :: Bool
    } deriving (Show, Read)

buildNamedConstraint :: String -> TypedGraphMorphism a b -> Bool -> Constraint a b
buildNamedConstraint = Constraint

premise :: Constraint a b -> TypedGraph a b
premise = domain . morphism

conclusion :: Constraint a b -> TypedGraph a b
conclusion = codomain . morphism

-- | Given a TypedGraph @G@ and a Constraint @a : P -> C@, check whether @G@ satisfies the Constraint @a@
satisfiesConstraint :: TypedGraph a b -> Constraint a b -> Bool
satisfiesConstraint graph constraint = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findConstraintMorphisms (premise constraint) graph
    qs = findConstraintMorphisms (conclusion constraint) graph
    a = morphism constraint
    triangleCommutes = all (\p -> any (\q -> compose a q == p) qs) ps
    allPremisesAreSatisfied = if positive constraint then triangleCommutes else not triangleCommutes

satisfiesAllConstraints :: TypedGraph a b -> [Constraint a b] -> Bool
satisfiesAllConstraints graph constraints = all (satisfiesConstraint graph) constraints

findConstraintMorphisms :: TypedGraph a b -> TypedGraph a b -> [TypedGraphMorphism a b]
findConstraintMorphisms = findMonomorphisms
