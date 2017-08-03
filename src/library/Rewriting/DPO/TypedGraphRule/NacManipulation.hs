{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rewriting.DPO.TypedGraphRule.NacManipulation where

import Control.Monad.List

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import Base.Isomorphic
import           Category.TypedGraph
import Util.Monad


-- | Auxiliar structure and function to delete first-order NACs
data DeleteScheme = DisableDelete | Monomorphisms | InitialPushouts

deleteStep :: forall n e. DeleteScheme -> [TypedGraphMorphism n e] -> [TypedGraphMorphism n e] -> TGraphCat n e [TypedGraphMorphism n e]

deleteStep DisableDelete _ concreteNACs = return concreteNACs

deleteStep Monomorphisms modeledNACs concreteNACs = runListT $ do
  nn' <- pickFromList concreteNACs :: ListT (TGraphCat n e) (TypedGraphMorphism n e)
  guardM $ allM (`maintainTest` nn') modeledNACs
  return nn'
  where
    findMorph :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> TGraphCat n e [TypedGraphMorphism n e]
    findMorph f g = findMorphisms monic (codomain f) (codomain g)

    --it forces commuting
    --maintainTest f g = Prelude.null . filter (\morp -> compose f morp == compose match g) <$> findMorph f g
    maintainTest f g = Prelude.null <$> findMorph f g

deleteStep InitialPushouts modeledNACs concreteNACs = do
  ipoModeled <- mapM (fmap (\ (_, x, _) -> x) . calculateInitialPushout) modeledNACs
  ipoConcrete <- map (\(n,(_,x,_)) -> (n,x)) . zip concreteNACs <$> mapM calculateInitialPushout concreteNACs
  runListT $ do
    nn' <- pickFromList ipoConcrete
    guardM $ allM (\nn -> not <$> isIso nn (snd nn')) ipoModeled
    return (fst nn')

{-
verifyIsoBetweenMorphisms :: TypedGraphMorphism n e -> TypedGraphMorphism n e -> cat Bool
verifyIsoBetweenMorphisms n n' = not $ Prelude.null comb
  where
    findIso :: FindMorphism cat morph => (t -> Obj cat) -> t -> t -> cat [morph]
    findIso f x y = findMorphisms iso (f x) (f y)

    findIsoDom = findIso domain n n'
    findIsoCod = findIso codomain n n'
    comb = [(d,c) | d <- findIsoDom, c <- findIsoCod, n' <&> d == c <&> n]
-}

-- | Auxiliar structure and function to create first-order NACs
data CreateScheme = DisableCreate | Pushout | ShiftNACs

createStep :: CreateScheme -> TypedGraphMorphism n e -> [TypedGraphMorphism n e] -> TGraphCat n e [TypedGraphMorphism n e]
createStep DisableCreate _ _ = return []
createStep Pushout match modeledNACs = mapM (fmap snd . calculatePushout match) modeledNACs
createStep ShiftNACs match modeledNACs =
  concatMapM (nacDownwardShift match) modeledNACs
    {-where
      -- FIXME: does something break when matches are restricted to monic?
      -- conf is used only to indicate AnyMatches, that is the most generic case for nacDownwardShift
      conf = MorphismsConfig GenericMorphism undefined -}
