module TypedGraph.Constraint
( premise
, conclusion
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
    allPremisesAreSatisfied = all (satisfiesSingle qs a) ps

findConstraintMorphisms :: TypedGraph a b -> TypedGraph a b -> [TypedGraphMorphism a b]
findConstraintMorphisms = findMonomorphisms

satisfiesSingle :: [TypedGraphMorphism a b] -> TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
satisfiesSingle [] _ _ = False
satisfiesSingle (q:qs) a p = compose a q == p || satisfiesSingle qs a p
