{-# LANGUAGE TypeFamilies #-}

module Graph.RuleMorphism{- (
    RuleMorphism
  , ruleMorphism
  ) -}where

import Abstract.AdhesiveHLR
import Abstract.Morphism
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

matchesSndOrder :: GraphRule a b -> GraphRule a b -> [RuleMorphism a b]
matchesSndOrder l g =
  do
    let f (a,b,c) = RuleMorphism l g a b c
    matchesL <- matches MONO (codomain (left l)) (codomain (left g))
    matchesK <- matches MONO (domain (left l)) (domain (left g))
    matchesR <- matches MONO (codomain (right l)) (codomain (right g))
    return $ f (matchesL, matchesK, matchesR)

commutingMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
commutingMorphism a b = typedMorphism (codomain a) (codomain b) select
  where
    mats = matches ALL (codomain a) (codomain b)
    filt = filter (\m -> compose a m == b) mats
    select = if Prelude.null filt
               then error "Error when commuting morphisms" {-empty (domain (codomain a)) (domain (codomain b))-}
               else mapping (head filt)

instance AdhesiveHLR (RuleMorphism a b) where
  poc (RuleMorphism _ cod1 l1 k1 r1) (RuleMorphism dom2 _ l2 k2 r2) = (k,l')
     where
       (kl,gl) = poc l1 l2
       (kk,gk) = poc k1 k2
       (kr,gr) = poc r1 r2
       l = commutingMorphism kk (compose (left dom2) kl)
       r = commutingMorphism kk (compose (right dom2) kr)
       newRule = graphRule l r []
       k = RuleMorphism dom2 newRule kl kk kr
       l' = RuleMorphism newRule cod1 gl gk gr
  
  po (RuleMorphism _ cod1 l1 k1 r1) (RuleMorphism _ cod2 l2 k2 r2) = (m',r')
     where
       (rl,kl) = po l1 l2
       (rk,kk) = po k1 k2
       (rr,kr) = po r1 r2
       l = commutingMorphism kk (compose (left cod1) kl)
       r = commutingMorphism kk (compose (right cod1) kr)
       newRule = graphRule l r []
       m' = RuleMorphism cod2 newRule rl rk rr
       r' = RuleMorphism cod1 newRule kl kk kr
  
  -- TODO
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
      monomorphism (mappingLeft rm) &&
      monomorphism (mappingInterface rm) &&
      monomorphism (mappingRight rm)
    epimorphism rm =
      epimorphism (mappingLeft rm) &&
      epimorphism (mappingInterface rm) &&
      epimorphism (mappingRight rm)
    isomorphism rm =
      isomorphism (mappingLeft rm) &&
      isomorphism (mappingInterface rm) &&
      isomorphism (mappingRight rm)

instance Valid (RuleMorphism a b) where
    valid (RuleMorphism dom cod mapL mapK mapR) =
        valid dom &&
        valid cod &&
        valid mapL &&
        valid mapK &&
        valid mapR &&
        compose mapK (left cod) == compose mapK (right cod) &&
        compose (left dom) mapL == compose (right dom) mapR
