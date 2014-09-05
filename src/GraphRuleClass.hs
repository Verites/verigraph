{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module GraphRuleClass where

import TypedMorphismClass

class TypedMorphismClass (T r) => GraphRuleClass r where
    type T r :: *

    left     :: r -> (T r)
    right    :: r -> (T r)

    graphRule :: T r -> T r -> r
