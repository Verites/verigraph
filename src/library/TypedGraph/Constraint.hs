module TypedGraph.Constraint
( Constraint
, satisfiesConstraint
) where

import           Abstract.Morphism
import           TypedGraph.Graph
import           TypedGraph.Morphism

type Constraint a b = TypedGraphMorphism a b

premise :: Constraint a b -> TypedGraph a b
premise = domain

conclusion :: Constraint a b -> TypedGraph a b
conclusion = codomain

satisfiesConstraint :: TypedGraph a b -> Constraint a b -> Bool
satisfiesConstraint g a = Prelude.null ps || allPremisesAreSatisfied
  where
    ps = findConstraintMorphisms (premise a) g
    qs = findConstraintMorphisms (conclusion a) g
    allPremisesAreSatisfied = all (\p -> any (\q -> compose a q == p) qs) ps

findConstraintMorphisms :: TypedGraph a b -> TypedGraph a b -> [TypedGraphMorphism a b]
findConstraintMorphisms = findMonomorphisms
