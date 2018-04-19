{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Category.TypedGraph.CompleteSpec where

import           Control.Monad
import           Data.Proxy
import           Test.Hspec
import           Test.HUnit

import           Abstract.Category
import           Abstract.Category.Limit
import           Base.Valid
import           Category.TypedGraph      ()
import qualified Data.Graphs              as G
import qualified Data.Graphs.Morphism     as G
import           Data.TypedGraph          as TG
import           Data.TypedGraph.Morphism
import           GrLang.Value
import           Util.Test
import qualified XML.GGXReader            as XML

tg = makeTypeGraph ["N"] [("E", "N", "N")]

spec :: Spec
spec = do
  describe "isFinal" $ do
    it "is true for finalObject" $
      let someMorphism = identity @GrMorphism (parseGraph tg "")
      in isFinal (Proxy @GrMorphism) (finalObject someMorphism) `shouldBe` True
    it "is true for a graph with a single loop" $
      isFinal (Proxy @GrMorphism) (parseGraph tg "n : N; n -:E-> n")
    it "is false for other graphs" $
      let graphs = map (parseGraph tg)
            [ "n n': N; n -:E-> n"
            , "n : N"
            , "n : N; n -e1 e2:E-> n"
            ]
      in forM_ graphs $ \g ->
        isFinal (Proxy @GrMorphism) g `shouldBe` False

  describe "morphismToFinalFrom" $ do
    let graphs = map (parseGraph tg)
          [ "n n': N; n -:E-> n"
          , "n : N"
          , "n : N; n -e1 e2:E-> n"
          ]
    it "should be valid" .
      forM_ graphs $ \graph ->
        validate (morphismToFinalFrom @GrMorphism graph) `shouldBe` IsValid
    it "should have correct domain" .
      forM_ graphs $ \g ->
        domain (morphismToFinalFrom @GrMorphism g) `shouldBe` g
    it "should have correct codomain" .
      forM_ graphs $ \g ->
        codomain (morphismToFinalFrom @GrMorphism g) `shouldBe` finalObject (identity @GrMorphism g)

  describe "calculateProduct" $ do
    let expectProduct (g', h') gh' p1' p2' = do
          let [g, h, gh] = map (parseGraph tg) [g', h', gh']
          let (p1, p2) = calculateProduct g h
          assertIsomorphic "domain p1" gh (domain p1)
          assertIsomorphic "p1" (parseMorphism gh g p1') p1
          assertEqual "domain p2" (domain p1) (domain p2)
          assertIsomorphic "p2" (parseMorphism gh h p2') p2

    it "should produce 'pairs' of nodes" $
      expectProduct ("a b : N", "c d : N")
        "ac ad bc bd : N"
        "ac ad -> a; bc bd -> b"
        "ac bc -> c; ad bd -> d"

    it "should produce 'pairs' of edges" $
      expectProduct ("a b : N; a -f:E-> b", "c d e : N; c -g:E-> d; d -h:E-> e")
        "ac ad ae bc bd be : N; ac -fg:E-> bd; ad -fh:E-> be"
        "ac ad ae -> a; bc bd be -> b; fg fh -> f"
        "ac bc -> c; ad bd -> d; ae be -> e; fg -> g; fh -> h"

  describe "calculateEqualizer" $ do
    let expectEqualizer (x', y', textF, textG) obj' expectedE = do
          let [x, y, obj] = map (parseGraph tg) [x', y',obj']
          let (f, g) = (parseMorphism x y textF, parseMorphism x y textG)
          let e = calculateEqualizer f g
          assertIsomorphic "domain e" obj (domain e)
          assertIsomorphic "e" (parseMorphism obj x expectedE) e

    it "should omit only nodes where morphisms 'disagree'" $
      expectEqualizer
        ("a b : N", "c d e : N", "a->c; b->d", "a->c; b->e")
        "a : N" "a->a"

    it "should omit only edges where morphisms 'disagree'" $
      expectEqualizer
        ( "a : N; a -f g:E-> a", "a : N; a -h i j:E-> a"
        , "a->a; f->h; g->i", "a->a; f->h; g->j" )
        "a : N; a -f:E-> a" "a->a; f->f"


  describe "calculatePullback" $ do
    let expectPullback (x', y', z', textF, textG) obj' expectedG' expectedF' = do
          let [x, y, z, obj] = map (parseGraph tg) [x', y', z', obj']
          let (f, g) = (parseMorphism x z textF, parseMorphism y z textG)
          let (f', g') = calculatePullback f g
          assertIsomorphic "domain f'" obj (domain f')
          assertIsomorphic "f'" (parseMorphism obj y expectedF') f'
          assertEqual "domain g'" (domain f') (domain g')
          assertIsomorphic "g'" (parseMorphism obj x expectedG') g'

    it "should omit elements outside the intersection of the images" $
      expectPullback
        ( "a b : N; a -f:E-> a", "c d : N; c -g:E-> c"
        , "ac b d e : N; ac -fg h:E-> ac"
        , "a->ac; b->b; f->fg", "c->ac; d->d; g->fg" )
        "ac : N; ac -fg:E-> ac"
        "ac -> a; fg -> f"
        "ac -> c; fg -> g"

    it "should duplicate elements with multiple preimages" $
      expectPullback
        ( "a b : N; a -f:E-> a", "c d : N; c -g:E-> c"
        , "abc d e : N; abc -fg:E-> abc"
        , "a b->abc; f->fg", "c->abc; d->d; g->fg" )
        "ac bc : N; ac -fg:E-> ac"
        "ac->a; bc->b; fg->f"
        "ac bc-> c; fg -> g"
