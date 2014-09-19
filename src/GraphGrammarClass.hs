{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module GraphGrammarClass where

import GraphRuleClass
import TypedMorphismClass
import MorphismClass

class GraphRuleClass (R gg) => GraphGrammarClass gg where

    type R gg :: *
    type K gg :: *

    typeGraph    :: gg -> (M (T (R gg)))
    initialGraph :: gg -> G (M (T (R gg)))
    rules        :: gg -> [(K gg, R gg)]
    
    graphGrammar :: (M (T (R gg))) -> G (M (T (R gg))) -> [(K gg, R gg)] -> gg
