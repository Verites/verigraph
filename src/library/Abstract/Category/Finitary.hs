{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleInstances #-}
module Abstract.Category.Finitary
  (
  -- * \(\mathcal{M}\)-Finitary
    MFinitary(..)
  , isInclusion
  -- *  \(\mathcal{E}\)-Cofinitary and \(\mathcal{E}'\)-pair cofinitary
  , ECofinitary(..)
  , isSurjection
  , E'PairCofinitary(..)
  ) where

import Abstract.Category
import Abstract.Category.Limit

{- | Type class for categories where each object has a finite number of
 \(\mathcal{M}\)-subobjects, which are called \(\mathcal{M}\)-finitary.

 In many categories, a fixed class \(\mathcal{M}\) of monomorphisms provides a
 natural notion of "inclusion". Examples are: in the category of (typed) graphs,
 the class of all monomorphisms provides this notion; in the category of
 symbolic graphs, the class of regular monomorphisms is appropriate.

 In these categories, there is a natural notion of subobject: given an inclusion
 \(f : A \to B\) (i.e. \(f\in \mathcal{M}\)), the equivalence class of all
 inclusions into \(B\) that are isomorphic to it can be interpreted as a
 subobject of \(B\). In practice, we often use the term subobject more loosely,
 referring also to single members of the equivalence class.

 In Verigraph, besides having a finite number of \(\mathcal{M}\)-subobjects,
 \(\mathcal{M}\)-finitary categories have an additional requirement: subobjects
 must have a canonical form. That is, given any inclusion, it must be possible
 to compute its canonical form, such that isomorphic inclusions have the same
 canonical form.
-}
class Category morph => MFinitary morph where
  -- | A class of morphisms whose equivalence classes define subobjects of their
  -- codomains. Usually denoted by \(\mathcal{M}\). Must be a subclass of 'monic'.
  inclusion :: MorphismClass morph

  -- | Construct a canonical version of the inclusion. The behaviour is
  -- undefined when the given morphism is not in the class of 'subobject's.
  --
  -- Given the inclusion \(f : A \to B \), returns a canonical version of it
  -- \(\hat{f} : B' \to B\).
  getCanonicalSubobject :: morph -> morph
  getCanonicalSubobject = snd . getMorphismToCanonicalSubobject

  -- | Construct a canonical version of the given inclusion, as well as an
  -- isomorphism to its domain. The behaviour is undefined when the
  -- given morphism is not in the class \(\mathcal{M}\) of inclusions.
  --
  -- Given the inclusion \(f : A \to B \), returns the pair \((h,\hat{f})\)
  -- where \(\hat{f} : B' \to B\) is the canonical subobject for \(f\) and
  -- \(h : A \to B'\) the appropriate isomorphism such that
  -- \(f = \hat{f} \circ h\).
  getMorphismToCanonicalSubobject :: morph -> (morph, morph)

  -- | Obtain the finite list of all subobjects of a given object, where all
  -- subobjects are in the canonical form.
  findAllSubobjectsOf :: Obj morph -> [morph]

-- | Check if the given morphism belongs to the class \(\mathcal{M}\) of
-- inclusions or subobjects of the category.
isInclusion :: forall morph. MFinitary morph => morph -> Bool
isInclusion = (`belongsToClass` inclusion @morph)
{-# INLINE isInclusion #-}

{- | Type class for categories where each object has a finite number of
 \(\mathcal{E}\)-quotients, which are called \(\mathcal{E}\)-cofinitary.

 In many categories, a fixed class \(\mathcal{E}\) of epimorphisms provides a
 natural notion of "surjection", or collapsing" of some elements. Examples are:
 in the category of (typed) graphs, the class of all epimorphisms provides this
 notion; in the  category of symbolic graphs, the class of regular epimorphisms
 is appropriate.

 In these categories, there is a natural notion of quotient (dual of subobject):
 given a surjection \(f : B \to C\) (i.e. \(f\in \mathcal{E}\)), the equivalence
 class of all surjections from \(B\) that are isomorphic to it can be
 interpreted as a quotient of \(B\). In practice, we often use the term quotient
 more loosely, referring also to single members of the equivalence class.

 In Verigraph, besides having a finite number of \(\mathcal{E}\)-quotients for each object,
 \(\mathcal{E}\)-cofinitary categories have an additional requirement: quotients
 must have a canonical form. That is, given any surjection, it must be possible
 to compute its canonical form, such that isomorphic surjections have the same
 canonical form.

 Examples are the category of typed graphs, where \(\mathcal{E}\) contains all
 epimorphisms, and the category of symbolic graphs, where \(\mathcal{E}\)
 contains all regular epimorphisms.
-}
class Category morph => ECofinitary morph where
  -- | A class of morphisms whose equivalence classes define quotients of their
  -- domains. Usually denoted by \(\mathcal{E}\). Must be a subclass of 'epic'.
  surjection :: MorphismClass morph

  -- | Construct a canonical version of the surjection. The behaviour is
  -- undefined when the given morphism is not in the class of surjections.
  --
  -- Given the surjection \(f : B \to C \), returns a canonical version of it
  -- \(\hat{f} : B \to B'\).
  getCanonicalQuotient :: morph -> morph
  getCanonicalQuotient = fst . getMorphismFromCanonicalQuotient

  -- | Construct a canonical version of the given surjection, as well as an
  -- isomorphism to its codomain. The behaviour is undefined when the
  -- given morphism is not in the class of surjections.
  --
  -- Given the surjection \(f : B \to C \), returns the pair \((\hat{f}, h)\)
  -- where \(\hat{f} : B \to B'\) is the canonical quotient for \(f\) and
  -- \(h : B' \to C\) the appropriate isomorphism such that
  -- \(f = h \circ \hat{f}\).
  getMorphismFromCanonicalQuotient :: morph -> (morph, morph)

  -- | Obtain the finite list of all quotients of a given object, where all
  -- quotients are in the canonical form.
  findAllQuotientsOf :: Obj morph -> [morph]

-- | Check if the given morphism belongs to the class \(\mathcal{E}\) of
-- surjections or quotients of the category.
isSurjection :: forall morph. ECofinitary morph => morph -> Bool
isSurjection = (`belongsToClass` surjection @morph)
{-# INLINE isSurjection #-}

{- | Type class for categories where each pair of objects has a finite number of
  joint surjections, up to isomorphism.
  
  We denote by \(\mathcal{E}'\) the class of /pairs/ of morphisms that are joint
  surjections, which must contain only jointly epic pairs. A pair of morphisms
  \(X \overset{f}{\to} Z \overset{g}{\leftarrow} Y\) is jointly epic if, given
  any two morphisms \(h_1, h_2 : Z \to A\), we have that \(h_1 \neq h_2\)
  implies \(h_1 \circ f \neq h_2 \circ f\) or \(h_1 \circ g \neq h_2 \circ g\).

  Any \(\mathcal{E}\)-cofinitary category with coproducts has this property. In
  this case, for any two objects \(X\) and \(Y\), there is a finite number of
  \(\mathcal{E}\)-morphisms with domain \(X + Y\) (up to isomorphism). The class
  \(\mathcal{E}'\) is then defined as all pairs of morphisms that can be
  obtained by composing coproduct injections with \(\mathcal{E}\)-morphisms.

  @  
        jX           jY
     X ────▶ X + Y ◀──── Y
      ╲        │        ╱
    eX ╲      e│       ╱ eY
        ╲      ▼      ╱
         ╲───▶ Z ◀───╱
  @
-}
class Category morph => E'PairCofinitary morph where

  -- | Check if the given pair of morphisms \(X \overset{f}{\to} Z
  -- \overset{g}{\leftarrow} Y\) belongs to the class \(\mathcal{E}'\) of joint
  -- surjections. The behaviour is undefined if the given morphisms have
  -- different codomains.
  isJointSurjection :: (morph, morph) -> Bool

  -- | Find all joint surjections from the given objects (up to isomorphism),
  -- such that the morphisms are in the appropriate classes.
  --
  -- The call \(\mathtt{findJointSurjections}~(\mathcal{C}_1, X)~(\mathcal{C}_2, Y)\)
  -- will generate a list containing all pairs of morphisms
  -- \(X \overset{f}{\to} Z \overset{g}{\leftarrow} Y\), such that \((f,g) \in \mathcal{E}'\),
  -- \(f \in \mathcal{C}_1\) and \(g \in \mathcal{C}_2\).
  findJointSurjections :: (MorphismClass morph, Obj morph) -> (MorphismClass morph, Obj morph) -> [(morph, morph)]
  default findJointSurjections :: (ECofinitary morph, Cocomplete morph) => (MorphismClass morph, Obj morph) -> (MorphismClass morph, Obj morph) -> [(morph, morph)]
  findJointSurjections (c1, x) (c2, y) = 
    let (jx, jy) = calculateCoproduct x y
    in [ (f, g) 
          | e <- findAllQuotientsOf (codomain jx) 
          , let (f, g) = (e <&> jx, e <&> jy)
          , f `belongsToClass` c1
          , g `belongsToClass` c2]


  -- | Find all joint surjections from the codomains of the given morphisms,
  -- such that their components are in the appropriate classes and the resulting
  -- square commutes.
  --
  -- The call \(\mathtt{findJointSurjectionSquares}~(\mathcal{C}_1, f)~(\mathcal{C}_2, g)\),
  -- where \(f : X \to A\) and \(g : X \to B\), will generate a list containing all
  -- pairs of morphisms \(A \overset{f'}{\to} C \overset{g'}{\leftarrow} B\), such that
  -- \((f',g') \in \mathcal{E}'\), \(f' \in \mathcal{C}_1\), \(g' \in \mathcal{C}_2\)
  -- and the following square commutes.
  --
  --         @
  --                g
  --             X──────▶B
  --             │       │
  --           f │       │ f'
  --             ▼       ▼
  --             A──────▶C
  --                g'
  --         @
  --
  -- __NOTE:__ The order of the morphism classes is different from 'findJointSurjections'.
  findJointSurjectionSquares :: (MorphismClass morph, morph) -> (MorphismClass morph, morph) -> [(morph, morph)]
  findJointSurjectionSquares (cf, f) (cg, g) =
    [ (f', g')
        | (g', f') <- findJointSurjections (cf, codomain f) (cg, codomain g) 
        , g' <&> f == f' <&> g ]
