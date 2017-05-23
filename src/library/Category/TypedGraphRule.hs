{-# LANGUAGE TypeFamilies #-}

module Category.TypedGraphRule
(

) where

import           Abstract.Category.FinitaryCategory
import           Abstract.Rewriting.DPO
import           Data.TypedGraph.Morphism
import           SndOrder.Morphism.Core

instance FinitaryCategory (RuleMorphism a b) where
    type Obj (RuleMorphism a b) = Production (TypedGraphMorphism a b)

    domain = rmDomain
    codomain = rmCodomain
    t2 <&> t1 = RuleMorphism (domain t1)
                 (codomain t2)
                 (mappingLeft t2 <&> mappingLeft t1)
                 (mappingInterface t2 <&> mappingInterface t1)
                 (mappingRight t2 <&> mappingRight t1)

    identity t = RuleMorphism t t
             (idMap (codomain (getLHS t)) (codomain (getLHS t)))
             (idMap (domain (getLHS t)) (domain (getLHS t)))
             (idMap (codomain (getRHS t)) (codomain (getRHS t)))

    isMonomorphism rm =
      isMonomorphism (mappingLeft rm) &&
      isMonomorphism (mappingInterface rm) &&
      isMonomorphism (mappingRight rm)

    isEpimorphism rm =
      isEpimorphism (mappingLeft rm) &&
      isEpimorphism (mappingInterface rm) &&
      isEpimorphism (mappingRight rm)

    isIsomorphism (RuleMorphism dom cod mapL mapK mapR) =
      isIsomorphism mapL &&
      isIsomorphism mapK &&
      isIsomorphism mapR &&
      mapL <&> getLHS dom == getLHS cod <&> mapK &&
      mapR <&> getRHS dom == getRHS cod <&> mapK
