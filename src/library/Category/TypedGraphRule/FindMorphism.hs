module Category.TypedGraphRule.FindMorphism () where

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Abstract.Rewriting.DPO
import           Category.TypedGraph                ()
import           Category.TypedGraphRule.Category

instance FindMorphism (RuleMorphism n e) where
  -- | A match between two first-order rules (desconsidering the NACs)
  findMorphisms cls' p1 p2 = 
    [ ruleMorphism p1 p2 fL fK fR
        | fK <- findMorphisms cls (interfaceObject p1) (interfaceObject p2)
        , fL <- findSpanCommuters cls (leftMorphism p1) (leftMorphism p2 <&> fK) 
        , fR <- findSpanCommuters cls (rightMorphism p1) (rightMorphism p2 <&> fK) ]
    where cls = toFstOrderMorphismClass cls'

  induceSpanMorphism = error "induceSpanMorphism not implemented for RuleMorphism"

  -- Given span (p2 <-f- p1 -g-> p3), returns a list of (h : p2 -> p3) with (h <&> f = g)
  findSpanCommuters cls' f g =
    [ ruleMorphism p2 p3 hL hK hR
        | hK <- findSpanCommuters cls (mappingInterface f) (mappingInterface g)
        , hL <- findSpanCommuters cls (leftMorphism p2) (leftMorphism p3 <&> hK)
        , hL <&> fL == gL
        , hR <- findSpanCommuters cls (rightMorphism p2) (rightMorphism p3 <&> hK) 
        , hR <&> fR == gR ]
    where
      (p2, p3) = (codomain f, codomain g)
      (fL, fR) = (mappingLeft f, mappingRight f)
      (gL, gR) = (mappingLeft g, mappingRight g)
      cls = toFstOrderMorphismClass cls'

  -- Given cospan (p2 -f-> p1 <-g- p3), returns a list of (h : p2 -> p3) with (f = g <&> h)
  findCospanCommuters cls' f g = 
    [ ruleMorphism p2 p3 hL hK hR
        | hK <- findCospanCommuters cls (mappingInterface f) (mappingInterface g)
        , hL <- findSpanCommuters cls (leftMorphism p2) (leftMorphism p3 <&> hK)
        , fL == gL <&> hL
        , hR <- findSpanCommuters cls (rightMorphism p2) (rightMorphism p3 <&> hK) 
        , fR == gR <&> hR ]
    where
      (p2, p3) = (domain f, domain g)
      (fL, fR) = (mappingLeft f, mappingRight f)
      (gL, gR) = (mappingLeft g, mappingRight g)
      cls = toFstOrderMorphismClass cls'
