{-# LANGUAGE TypeFamilies #-}

module Graph.RuleMorphism{- (
    RuleMorphism
  , ruleMorphism
  ) -}where

import Abstract.AdhesiveHLR
import Abstract.Morphism as M
import Abstract.Valid
import Graph.GraphRule
import Graph.TypedGraphMorphism

data RuleMorphism a b =
  RuleMorphism {
    getDomain        :: GraphRule a b
  , getCodomain      :: GraphRule a b
  , mappingLeft      :: TypedGraphMorphism a b
  , mappingInterface :: TypedGraphMorphism a b
  , mappingRight     :: TypedGraphMorphism a b
  } deriving (Show)

ruleMorphism :: GraphRule a b -> GraphRule a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> TypedGraphMorphism a b
             -> RuleMorphism a b
ruleMorphism = RuleMorphism

instance AdhesiveHLR (RuleMorphism a b) where
  -- FIXME
  poc m l = (m,l)
  
  -- FIXME
  po k r = (k,r)
  
  -- FIXME
  injectivePullback f g = (f,g)

-- FIXME
instance Eq (RuleMorphism a b) where
    (RuleMorphism _ _ mapL1 mapK1 mapR1) == (RuleMorphism _ _ mapL2 mapK2 mapR2) =
        {-dom1 == dom2 &&
        cod1 == cod2 &&-}
        mapL1 == mapL2 &&
        mapK1 == mapK2 &&
        mapR1 == mapR2

instance Morphism (RuleMorphism a b) where
    type Obj (RuleMorphism a b) = GraphRule a b

    domain = getDomain
    codomain = getCodomain
    compose t1 t2 =
        RuleMorphism (domain t1)
                     (codomain t2)
                     (compose (mappingLeft t1) (mappingLeft t2))
                     (compose (mappingInterface t1) (mappingInterface t2))
                     (compose (mappingRight t1) (mappingRight t2))
    
    id t = RuleMorphism t t
             (idMap (codomain (left t)) (codomain (left t)))
             (idMap (domain (left t)) (domain (left t)))
             (idMap (codomain (right t)) (codomain (right t)))

    monomorphism rm =
      (monomorphism (mappingLeft rm)) &&
      (monomorphism (mappingInterface rm)) &&
      (monomorphism (mappingRight rm))
    epimorphism rm =
      (epimorphism (mappingLeft rm)) &&
      (epimorphism (mappingInterface rm)) &&
      (epimorphism (mappingRight rm))
    isomorphism rm =
      (isomorphism (mappingLeft rm)) &&
      (isomorphism (mappingInterface rm)) &&
      (isomorphism (mappingRight rm))

instance Valid (RuleMorphism a b) where
    valid (RuleMorphism dom cod mapL mapK mapR) =
        valid dom &&
        valid cod &&
        valid mapL &&
        valid mapK &&
        valid mapR &&
        (compose mapK (left cod)) == (compose mapK (right cod)) &&
        (compose (left dom) mapL) == (compose (right dom) mapR)
