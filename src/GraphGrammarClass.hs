{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module GraphGrammarClass where

import GraphRuleClass
import TypedMorphismClass
import MorphismClass

class GraphRuleClass (R gg) => GraphGrammarClass gg where

    type R gg :: *
    type K gg :: *

    typeGraph    :: gg -> (T gg)
    rules        :: gg -> [(K gg, R gg)]
    initialGraph :: gg -> G (M (T (R gg)))
    
    graphGrammar :: T gg -> G (M (T (R gg))) -> [(K gg, R gg)] -> gg
