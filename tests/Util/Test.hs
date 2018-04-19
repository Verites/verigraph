{-# LANGUAGE FlexibleContexts #-}
module Util.Test
  ( forAllMorphismsBetween
  , equalLists
  , (-->)
  , assertIsomorphic
  , shouldBeIsomorphicTo
  , shouldBeIsomorphicToList
  , TypeGraph
  , GrGraph
  , GrMorphism
  , GrRule
  , makeTypeGraph
  , parseGraph
  , parseMorphism
  , parseRule
  ) where

import           Control.DeepSeq
import qualified Control.Exception              as E
import           Control.Monad
import           Data.CallStack
import qualified Data.List                      as List
import           Test.Hspec
import           Test.HUnit.Lang                (FailureReason (..), HUnitFailure (..))
import           Test.QuickCheck                as QuickCheck

import           Abstract.Category
import           Abstract.Category.FindMorphism
import           Abstract.Category.Finitary
import           Base.Isomorphic
import           GrLang.TestUtils
import           GrLang.Value


forAllMorphismsBetween :: (QuickCheck.Testable prop, FindMorphism a, Show a) =>
  MorphismClass a -> Obj a -> Obj a -> (a -> prop) -> Property
forAllMorphismsBetween restriction g1 g2 f =
  let
    morphisms = findMorphisms restriction g1 g2
  in
    morphisms /= [] ==> forAll (elements morphisms) f

-- | Tests if the given lists have the same elements, insensitive to order
equalLists :: Eq a => [a] -> [a] -> Bool
equalLists [] [] = True
equalLists _ [] = False
equalLists [] _ = False
equalLists (x:xs) ys =
  case List.break (==x) ys of
    (_, []) -> False -- x not found in ys
    (ys0, _:ys1) -> equalLists xs (ys0 ++ ys1)

-- | Boolean implication.
(-->) :: Bool -> Bool -> Bool
False --> _ = True
True --> a = a

infixr 0 -->


assertIsomorphic :: (HasCallStack, Iso a, Show a) => String -> a -> a -> Expectation
assertIsomorphic preface expected actual =
  unless (expected ~= actual) $
    prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO
      (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg)
    where
      prefaceMsg
        | null preface = Nothing
        | otherwise = Just preface
      expectedMsg = show expected
      actualMsg = show actual

shouldBeIsomorphicTo :: (Iso a, Show a) => a -> a -> Expectation
actual `shouldBeIsomorphicTo` expected = assertIsomorphic "" expected actual

shouldBeIsomorphicToList :: (Iso a, Show a) => [a] -> [a] -> Expectation
shouldBeIsomorphicToList actual expected =
  let (unexpected, unobtained) = findMismatch actual expected
  in unless (List.null unexpected && List.null unobtained) $
    expectationFailure . List.intercalate "\n" . concat $
      [ if not (List.null unexpected) then
          "got unexpected items: " : map (("  "++) . show) unexpected
        else []
      , if not (List.null unobtained) then
          "didn't get expected items: " : map (("  "++) . show) unobtained
        else []
      , [ "expected: " ++ show expected, " but got: " ++ show actual ]
      ]
  where
    findMismatch [] [] = ([], [])
    findMismatch [] expected = ([], expected)
    findMismatch actual [] = (actual, [])
    findMismatch (x:xs) ys = case break (~=x) ys of
      (ys1, _:ys2) -> findMismatch xs (ys1 ++ ys2)
      (_, []) ->
        let (unexpected, unobtained) = findMismatch xs ys
        in (x : unexpected, unobtained)

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

