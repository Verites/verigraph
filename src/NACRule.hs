{-# LANGUAGE TypeFamilies #-}

module NACRule (NACRule) where

import GraphRule (GraphRule)
import GraphRuleClass
import TypedMorphism (TypedMorphism)
import TypedMorphismClass
import MorphismClass
import Morphism (Morphism)
import NACRuleClass

data NACRule a b = NACRule {
                        getRule  :: GraphRule a b
                      , getNACs  :: [TypedMorphism a b]
                   }

instance NACRuleClass (NACRule a b) where
    type R (NACRule a b) = GraphRule a b

    rule = getRule
    nacs = getNACs

    nacRule = NACRule
