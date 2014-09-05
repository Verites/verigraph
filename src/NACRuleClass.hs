{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module NACRuleClass where

import GraphRuleClass
import TypedMorphismClass

class GraphRuleClass (R n) => NACRuleClass n where
    type R n :: *

    rule     :: n -> (R n)
    nacs     :: n -> [T (R n)]

    nacRule  :: R n -> [T (R n)] -> n
