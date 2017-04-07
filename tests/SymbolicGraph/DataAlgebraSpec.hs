{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SymbolicGraph.DataAlgebraSpec where


import           SymbolicGraph.DataAlgebra.Parser
import           SymbolicGraph.DataAlgebra.QuickCheck ()


import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec                          (parse)
import           Text.PrettyPrint.Leijen.Text         (displayT, pretty,
                                                       renderPretty)


spec :: Spec
spec = do

  it "always parses correctly after pretty printing" $
    property $ \expr ->
      let text = displayT . renderPretty 0.8 100 $ pretty expr
      in parse expr' "<test>" text == Right expr
