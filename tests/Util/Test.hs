module Util.Test where

import           Control.Monad
import qualified Data.List                          as List
import           Test.Hspec
import           Test.QuickCheck                    as QuickCheck

import           Abstract.Category.FinitaryCategory
import           Base.Isomorphic


forAllMorphismsBetween :: (QuickCheck.Testable prop, FindMorphism a, Show a) =>
  MorphismType -> Obj a -> Obj a -> (a -> prop) -> Property
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


shouldBeIsomorphicTo :: (Iso a, Show a) => a -> a -> Expectation
shouldBeIsomorphicTo expected actual =
  unless (expected ~= actual)
    (expectationFailure . List.intercalate "\n" $
      ["expected: " ++ show expected, " but got: " ++ show actual])

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
