module GrLang.QuickCheck where

import qualified Data.Char       as Char
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Test.QuickCheck

import           Base.Annotation (Annotated (..), Located)
import           Base.Location
import           GrLang.AST

instance Arbitrary TopLevelDeclaration where
  arbitrary = oneof
    [ Import <$> located genFilePath
    , DeclNodeType <$> located genIdentifier
    , DeclEdgeType <$> located genIdentifier <*> located genIdentifier <*> located genIdentifier
    , DeclGraph <$> located genIdentifier <*> listOf arbitrary ]

  shrink (DeclGraph g body) = [ DeclGraph g body' | body' <- shrink body ]
  shrink _                  = []

instance Arbitrary GraphDeclaration where
  arbitrary = oneof
    [ DeclNodes <$> listOf1 (located genIdentifier) <*> located genIdentifier
    , DeclEdges <$> located genIdentifier <*> arbitrary <*> located genIdentifier ]

  shrink (DeclNodes ns t)   = [ DeclNodes ns' t | ns' <- shrinkList1 ns ]
  shrink (DeclEdges s es t) = [ DeclEdges s es' t | es' <- shrink es ]

instance Arbitrary ParallelEdgesDeclaration where
  arbitrary = oneof
    [ SingleType <$> listOf2 (located genIdentifier) <*> located genIdentifier
    , MultipleTypes <$> listOf1 (located edge) ]
    where
      edge = (,) <$> optional genIdentifier <*> genIdentifier
      listOf2 gen = (:) <$> gen <*> listOf1 gen

  shrink (SingleType es t)  = [ SingleType es' t | es' <- shrinkList1 es ]
  shrink (MultipleTypes es) = [ MultipleTypes es' | es' <- shrinkList1 es ]

shrinkList1 :: [a] -> [[a]]
shrinkList1 []  = []
shrinkList1 [_] = []
shrinkList1 l   = filter (not . null) $ shrinkList (:[]) l

optional :: Gen a -> Gen (Maybe a)
optional gen = oneof [ Just <$> gen, pure Nothing ]

located :: Gen a -> Gen (Located a)
located x = A <$> optional (Location "QuickCheck" <$> arbitrary) <*> x

instance (Arbitrary info, Arbitrary a) => Arbitrary (Annotated info a) where
  arbitrary = A <$> arbitrary <*> arbitrary
  shrink (A i x) = [ A i x' | x' <- shrink x ]

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink = fmap Text.pack . shrink . Text.unpack

genFilePath :: Gen String
genFilePath = listOf (arbitrary `suchThat` isValid)
  where
    isValid c = (Char.isAlphaNum c || c == ' ' || Char.isSymbol c) && Char.isAscii c && c /= '"' && c /= '\\'

genIdentifier :: Gen Text
genIdentifier =
  Text.pack <$> ((:) <$> startChar <*> listOf midChar)
  where
    startChar = elements ('?' : startChars)
    startChars = ['a'..'z'] ++ "_"

    midChar = elements midChars
    midChars = startChars ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'"
