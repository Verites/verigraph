module Rewriting.DPO.TypedGraphRule.NacManipulation where

import           Abstract.Category
import           Abstract.Category.Adhesive
import           Abstract.Category.FindMorphism
import           Abstract.Category.Limit
import           Abstract.Rewriting.DPO
import           Category.TypedGraph                     ()
import           Category.TypedGraphRule.Limit           ()
import           Data.TypedGraph.Morphism


-- | Auxiliar structure and function to delete first-order NACs
data DeleteScheme = DisableDelete | Monomorphisms | InitialPushouts

deleteStep :: DeleteScheme -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b]

deleteStep DisableDelete _ concreteNACs = concreteNACs

deleteStep Monomorphisms modeledNACs concreteNACs =
  [nn' | nn' <- concreteNACs, all (`maintainTest` nn') modeledNACs]
    where
      findMorph :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
      findMorph a b = findMorphisms monic (codomain a) (codomain b)

      --it forces commuting
      --maintainTest a b = Prelude.null $ filter (\morp -> compose a morp == compose match b) (findMorph a b)
      maintainTest a b = Prelude.null $ findMorph a b

deleteStep InitialPushouts modeledNACs concreteNACs =
  [fst nn' | nn' <- ipoConcrete, all (\nn -> not (verifyIsoBetweenMorphisms nn (snd nn'))) ipoModeled]
  where
    ipoModeled = map ((\ (_, x, _) -> x) . calculateMInitialPushout) modeledNACs
    ipoConcrete = map (\(n,(_,x,_)) -> (n,x)) (zip concreteNACs (map calculateMInitialPushout concreteNACs))

verifyIsoBetweenMorphisms :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> Bool
verifyIsoBetweenMorphisms n n' = not $ Prelude.null comb
  where
    findIso :: FindMorphism morph => (t -> Obj morph) -> t -> t -> [morph]
    findIso f x y = findMorphisms iso (f x) (f y)

    findIsoDom = findIso domain n n'
    findIsoCod = findIso codomain n n'
    comb = [(d,c) | d <- findIsoDom, c <- findIsoCod, n' <&> d == c <&> n]

-- | Auxiliar structure and function to create first-order NACs
data CreateScheme = DisableCreate | Pushout | ShiftNACs

createStep :: CreateScheme -> TypedGraphMorphism a b -> [TypedGraphMorphism a b] -> [TypedGraphMorphism a b]

createStep DisableCreate _ _ = []

createStep Pushout match modeledNACs =
  map (snd . calculatePushout match) modeledNACs

createStep ShiftNACs match modeledNACs =
  concatMap (nacDownwardShift conf match) modeledNACs
    where
      -- conf is used only to indicate AnyMatches, that is the most generic case for nacDownwardShift
      conf = MorphismsConfig anyMorphism
