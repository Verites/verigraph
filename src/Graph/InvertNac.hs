module Graph.InvertNac (
    inverse,
    leftShiftNac
  ) where

import           Analysis.GluingConditions (satsGluing)
import           Graph.GraphRule
import qualified Graph.Rewriting           as RW
import           Graph.TypedGraphMorphism

-- | Revert a Rule shifting NACs
inverse :: Bool -> GraphRule a b -> GraphRule a b
inverse inj r = graphRule (right r) (left r) (concatMap (leftShiftNac inj r) (nacs r))

-- | Given a rule @L <-l- K -r-> R@ and a Right NAC morphism @n : R -> N@, it shifts the NAC over the rule resulting in a list of Left NAC morphisms of type @n': L -> N'@
leftShiftNac :: Bool -> GraphRule a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
leftShiftNac inj rule n = [RW.comatch n rule | satsGluing inj (left rule) n]
