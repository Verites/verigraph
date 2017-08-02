{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
module Abstract.Category.NewClasses 
  (
    -- * Category
    Category(..)
  , isMonic
  , isEpic
  , isIsomorphism
  , Span
  , Cospan

  -- * \(\mathcal{M}\)-Finitary and \(\mathcal{E}\)-Cofinitary
  , MFinitary(..)
  , isSubobject
  , ECofinitary(..)
  , isQuotient

  -- * \((\mathcal{E},\mathcal{M})\)-Factorization
  , EM'Factorizable(..)
  , isMonicFactor
  , EM'PairFactorizable(..)

  -- * Finding Morphisms
  , FindMorphism(..)

  -- * Limits and Colimits
  , Complete(..)
  , Cocomplete(..)

  -- * Adhesive Categories
  , Adhesive(..)
  , LRNAdhesive(..)
  , isRuleMorphism
  , isLeftHandMorphism
  , isMatchMorphism

  -- * Other Constructions
  , InitialPushout(..)
  ) where

import Control.Monad.List
import           Data.List.NonEmpty (NonEmpty(..))

import Util.Monad


{- | Type class for representing categories in Verigraph.

Each category is defined by two types: a type of morphism @morph@ and a monad
@cat@ that provides context for computations within the category. Furthermore,
there is an associated type of objects @Obj cat@ and an associated type
representing the different classes of morphisms of the category @MorphismClass
cat@.

By making the computations related to a category run inside a monad, we allow
the same data structures to represent multiple categories according to different
"parameters". An example are the categories of typed graphs: for every type
graph \(T\), there is a category \(\mathbf{Graph}_T\) of graphs typed in it. In
this case, we can use a @Reader@ monad that provide the type graph as a "global
constant".

Having an associated data type that enumerates classes of morphisms is necessary
because different categories have different relevant classes of morphisms.
Although in the category of (typed) graphs it is sufficient to consider only
monomorphisms, epimorphisms and isomorphisms, other categories may need to
consider e.g. regular monomorphisms or other category-specific classes. An
example of this are categories of attributed graphs, where morphisms whose
algebra-component must be an isomorphism are relevant.
-}
class (Monad cat, Eq morph) => Category cat morph | cat -> morph, morph -> cat where
  -- | Data type that represents objects of the category
  type Obj cat :: *

  -- | Morphism composition operator.
  --
  -- Given morphisms \(f:B \to C\), and \(g:A\to B\), returns \(f \circ g : A\to C\),
  -- i.e. the order of arguments is the same as in function composition.
  (<&>)    :: morph -> morph -> morph

  -- | Given an object \(A\), return its identity morphism \(id_A : A\to A\).
  identity :: Obj cat -> morph
  -- | Given a morphism \(f:A \to B\), return the object \(A\).
  domain   :: morph -> Obj cat
  -- | Given a morphism \(f:A \to B\), return the object \(B\).
  codomain :: morph -> Obj cat

  -- | Data type defining the different classes of morphism for this category.
  data MorphismClass cat :: *
  -- | Class containing all morphisms of the category.
  anyMorphism :: MorphismClass cat
  -- | Class containing all monomorphisms of the category.
  monic :: MorphismClass cat
  -- | Class containing all epimorphisms of the category.
  epic :: MorphismClass cat
  -- | Class containing all isomorphisms of the category.
  iso :: MorphismClass cat

  -- | Check if a given morphism belongs to the given class of morphisms.
  belongsToClass :: morph -> MorphismClass cat -> cat Bool

  -- | Check if all morphisms of the first class belong also to the second class.
  isSubclassOf :: MorphismClass cat -> MorphismClass cat -> cat Bool

-- | Check if a given morphism is monic.
isMonic :: forall cat morph. Category cat morph => morph -> cat Bool
isMonic = (`belongsToClass` monic @cat)

-- | Check if a given morphism is epic.
isEpic :: forall cat morph. Category cat morph => morph -> cat Bool
isEpic = (`belongsToClass` epic @cat)

-- | Check if a given morphism is an isomorphism.
isIsomorphism :: forall cat morph. Category cat morph => morph -> cat Bool
isIsomorphism = (`belongsToClass` iso @cat)

-- | A span is a pair of morphisms with same domain (e.g. \(A \to C \leftarrow B \)).
type Span (cat :: * -> *) morph = (morph, morph)

-- | A cospan is a pair of morphisms with same codomain (e.g. \(A \leftarrow C \to B \)).
type Cospan (cat :: * -> *) morph = (morph, morph)

{- | Type class for categories where each object has a finite number of
 \(\mathcal{M}\)-subobjects, which are called \(\mathcal{M}\)-finitary.

 In many categories, a fixed class \(\mathcal{M}\) of monomorphisms provides a
 natural notion of "inclusion". Examples are: in the category of (typed) graphs,
 the class of all monomorphisms provides this notion; in the category of
 symbolic graphs, the class of regular monomorphisms is appropriate.

 In these categories, there is a natural notion of subobject: given an inclusion
 \(f : A \to B\) (i.e. \(f\in \mathcal{M}\)), the equivalence class of all
 inclusions that are isomorphic to it can be interpreted as a subobject of
 \(B\). In practice, we often use the term subobject more loosely, referring
 also to single members of the equivalence class.

 In Verigraph, besides having a finite number of \(\mathcal{M}\)-subobjects,
 \(\mathcal{M}\)-finitary categories have an additional requirement: subobjects
 must have a canonical form. That is, given any inclusion, it must be possible
 to compute its canonical form, such that isomorphic inclusions have the same
 canonical form.
-}
class Category cat morph => MFinitary cat morph where
  {-# MINIMAL subobject, getMorphismToCanonicalSubobject, findAllSubobjectsOf #-}

  -- | A class of morphisms whose equivalence classes define subobjects of their
  -- codomains. Usually denoted by \(\mathcal{M}\). Must be a subclass of 'monic'.
  subobject :: MorphismClass cat

  -- | Construct a canonical version of the subobject. The behaviour is
  -- undefined when the given morphism is not in the class of 'subobject's.
  --
  -- Given the inclusion \(f : A \to B \), returns a canonical version of it
  -- \(\hat{f} : B' \to B\).
  getCanonicalSubobject :: morph -> cat morph
  getCanonicalSubobject = fmap snd . getMorphismToCanonicalSubobject

  -- | Construct a canonical version of the given subobject, as well as an
  -- isomorphism to the canonical version. The behaviour is undefined when the
  -- given morphism is not in the class of 'subobject's.
  --
  -- Given the inclusion \(f : A \to B \), returns the pair \((h,\hat{f})\)
  -- where \(\hat{f} : B' \to B\) is the canonical subobject for \(f\) and
  -- \(h : A \to B'\) the appropriate isomorphism such that
  -- \(f = \hat{f} \circ h\).
  getMorphismToCanonicalSubobject :: morph -> cat (morph, morph)

  -- | Obtain the finite list of all subobjects of a given object, where all
  -- subobjects are in the canonical form.
  findAllSubobjectsOf :: Obj cat -> cat [morph]

-- | Check if the given morphism belongs to the class \(\mathcal{M}\) of
-- inclusions or subobjects of the category.
isSubobject :: forall cat morph. MFinitary cat morph => morph -> cat Bool
isSubobject = (`belongsToClass` subobject @cat)


{- | Type class for categories where each object has a finite number of
 \(\mathcal{E}\)-quotients, which are called \(\mathcal{E}\)-cofinitary.

 In many categories, a fixed class \(\mathcal{E}\) of monomorphisms provides a
 natural notion of "collapsing" of some elements. Examples are: in the category
 of (typed) graphs, the class of all epimorphisms provides this notion; in the
 category of symbolic graphs, the class of regular epimorphisms is appropriate.

 In these categories, there is a natural notion of quotient (dual of subobject):
 given a "collapsing" \(f : B \to C\) (i.e. \(f\in \mathcal{E}\)), the
 equivalence class of all collapsings that are isomorphic to it can be
 interpreted as a quotient of \(B\). In practice, we often use the term quotient
 more loosely, referring also to single members of the equivalence class.

 In Verigraph, besides having a finite number of \(\mathcal{E}\)-quotients,
 \(\mathcal{E}\)-cofinitary categories have an additional requirement: quotients
 must have a canonical form. That is, given any collapsing, it must be possible
 to compute its canonical form, such that isomorphic collapsings have the same
 canonical form.
-}
class Category cat morph => ECofinitary cat morph where
  {-# MINIMAL quotient, getMorphismFromCanonicalQuotient, findAllQuotientsOf #-}

  -- | A class of morphisms whose equivalence classes define quotients of their
  -- domains. Usually denoted by \(\mathcal{E}\). Must be a subclass of 'epic'.
  quotient :: MorphismClass cat

  -- | Construct a canonical version of the quotient. The behaviour is
  -- undefined when the given morphism is not in the class of 'quotient's.
  --
  -- Given the collapsing \(f : B \to C \), returns a canonical version of it
  -- \(\hat{f} : B \to B'\).
  getCanonicalQuotient :: morph -> cat morph
  getCanonicalQuotient = fmap fst . getMorphismFromCanonicalQuotient

  -- | Construct a canonical version of the given quotient, as well as an
  -- isomorphism from the canonical version. The behaviour is undefined when the
  -- given morphism is not in the class of 'quotient's.
  --
  -- Given the collapsing \(f : B \to C \), returns the pair \((\hat{f}, h)\)
  -- where \(\hat{f} : B \to B'\) is the canonical quotient for \(f\) and
  -- \(h : B' \to C\) the appropriate isomorphism such that
  -- \(f = h \circ \hat{f}\).
  getMorphismFromCanonicalQuotient :: morph -> cat (morph, morph)

  -- | Obtain the finite list of all quotients of a given object, where all
  -- quotients are in the canonical form.
  findAllQuotientsOf :: Obj cat -> cat [morph]

-- | Check if the given morphism belongs to the class \(\mathcal{E}\) of
-- collapsings or quotients of the category.
isQuotient :: forall cat morph. ECofinitary cat morph => morph -> cat Bool
isQuotient = (`belongsToClass` quotient @cat)

{- | Type class for categories that have \((\mathcal{E},\mathcal{M'})\)-factorization.

 The notion of \((\mathcal{E},\mathcal{M'})\)-factorization is a categorial
 generalization for the "image of a function". In set theory, given a function
 \(f : A \to B\), we have the image \(\mathrm{Im}(f) \subseteq B\). The function
 \(f\) can then be seen as a surjection whose codomain is \(\mathrm{Im}(f)\),
 and there is a natural inclusion for \(\mathrm{Im}(f) \subseteq B\).

 The categorial generalization amounts to the following. The category has two
 classes of morphisms \(\mathcal{E}\) (of "surjections") and \(\mathcal{M'}\)
 (of "inclusions"), such that any morphism \(f : A \to B\) can be decomposed
 into a pair of morphisms \(f_e : A \to B', f_m : B' \to B\) with
 \(f_e \in \mathcal{E}\) and \(f_m \in \mathcal{M'}\). Furthermore, any two
 factorizations of \(f\) are isomorphic, that is, given another factorization
 \(f_e' : A \to B'', f_m' : B' \to B\), there is a unique isomorphism between
 \(B'\) and \(B''\) making the following triangles commute.

 @
       fe              fm
    A ────▶ B'     B' ────▶ B
     ╲      ▲      ▲        ▲
      ╲     │      │       ╱
   fe' ╲    ▼      ▼      ╱ fm'
        ╲─▶ B''    B'' ──╱
 @

 In Verigraph, we require that the class \(\mathcal{E}\) be the class of
 epimorphisms for which the category is \(\mathcal{E}\)-cofinitary (see the type
 class 'ECofinitary'), and that the class \(\mathcal{M'}\) be a class of
 monomorphisms. Note that \(\mathcal{M'}\) may be different from the class
 \(\mathcal{M}\) for which the category is \(\mathcal{M}\)-finitary.
 
 Examples are: the category of (typed) graphs, where \(\mathcal{E}\) is the
 class of all epimorphisms and \(\mathcal{M'} = \mathcal{M}\) is the class of
 all monomorphisms; the category of symbolic graphs, where \(\mathcal{E}\) is
 the class of all regular epimorphisms and \(\mathcal{M'}\) is the class of all
 monomorphisms (note that \(\mathcal{M} \subset \mathcal{M'}\) is the class of
 regular monomorphisms).
-}
class ECofinitary cat morph => EM'Factorizable cat morph where

  -- | The class of monomorphisms \(\mathcal{M'}\) for which the category has
  -- \((\mathcal{E},\mathcal{M'})\)-factorization.
  monicFactor :: MorphismClass cat

  -- | Obtain the \((\mathcal{E},\mathcal{M'})\)-factorization of the given
  -- morphism.
  --
  -- Given the morphism \(f : A \to B\), obtain the pair \((f_e : A \to B', f_m : B' \to B)\)
  -- that is its \((\mathcal{E},\mathcal{M'})\)-factorization
  factorize :: morph -> cat (morph, morph)

-- | Check if the given morphism may be a monic factor of an \((\mathcal{E},\mathcal{M'})\)-factorization.
isMonicFactor :: forall cat morph. EM'Factorizable cat morph => morph -> cat Bool
isMonicFactor = (`belongsToClass` monicFactor @cat)

-- $jointly-epic
-- A pair of morphisms \(X \overset{f}{\to} Z \overset{g}{\leftarrow} Y\) is
-- jointly epic if, given any two morphisms \(h_1, h_2 : Z \to A\), we have that
-- \(h_1 \neq h_2\) implies \(h_1 \circ f \neq h_2 \circ f\) or \(h_1 \circ g \neq h_2 \circ g\).

class EM'Factorizable cat morph => EM'PairFactorizable cat morph where
  -- | Create all jointly epimorphic pairs of morphisms from the given objects,
  -- such that their components are in the appropriate classes.
  --
  -- The call \(\mathtt{findJointlyEpicPairs}~(\mathcal{C}_1, X)~(\mathcal{C}_2, Y)\)
  -- will generate a list containing all pairs of morphisms
  -- \(X \overset{f}{\to} Z \overset{g}{\leftarrow} Y\), such that:
  --
  --   * \((f,g)\) is jointly epic
  --   * \(f \in \mathcal{C}_1\)
  --   * \(g \in \mathcal{C}_2\)
  findJointlyEpicPairs :: (MorphismClass cat, Obj cat) -> (MorphismClass cat, Obj cat) -> cat [(morph, morph)]

  -- | Create all jointly epimorphic pairs of morphisms from the codomains of
  -- the given objects, such that their components are in the appropriate
  -- classes and the resulting square commutes.
  --
  -- The call \(\mathtt{findJointlyEpicSquares}~(\mathcal{C}_1, f)~(\mathcal{C}_2, g)\),
  -- where \(f : X \to A\) and \(g : X \to B\), will generate a list containing all
  -- pairs of morphisms \(A \overset{f'}{\to} C \overset{g'}{\leftarrow} B\), such that:
  --
  --     * \((f',g')\) is jointly epic
  --     * \(f' \in \mathcal{C}_1\)
  --     * \(g' \in \mathcal{C}_2\)
  --     * The following square commutes
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
  -- __NOTE:__ The order of the morphism classes is different from 'jointlyEpicPairs'.
  --
  -- A pair of morphisms /X --f-> Z <-g-- Y/ is jointly epic if, given any two
  -- morphisms /h1, h2 : Z -> A/, if /h1 != h2/ then /h1 . f != h2 . f/ or 
  -- /h2 . f != h2 . g/.
  findJointlyEpicSquares :: (MorphismClass cat, morph) -> (MorphismClass cat, morph) -> cat [(morph, morph)]
  findJointlyEpicSquares (cf, f) (cg, g) = runListT $ do
    (g', f') <- pickOne $ findJointlyEpicPairs (cf, codomain f) (cg, codomain g)
    guard (g' <&> f == f' <&> g)
    return (f', g')

{- | Type class for categories whose \(\mathrm{Hom}\)-sets are computable.

 Given objects \(A,B\) of a category, the set of morphisms from \(A\) to \(B\)
 is denoted by \(\mathrm{Hom}(A,B)\). In some categories, this set is
 computable. That is, given two objects, there is an algorithm that enumerates
 all morphisms between them.

 When finding morphisms between any two object, we are often only interested in
 some of the morphisms, e.g. only monomorphisms or only the morphisms that make
 a particular diagram commute. Since the number of morphisms between any two
 objects may be very large, enumerating all then filtering is very inefficient
 in general. Thus, there are specialized versions of morphism search that may be
 considerably faster. 
-}
class Category cat morph => FindMorphism cat morph where
  -- | Find all morphisms between the given objects that belong to the given class.
  findMorphisms :: MorphismClass cat -> Obj cat -> Obj cat -> cat [morph]

  -- | Given a span \(B \overset{f}{\leftarrow} A \overset{g}{\to} C\), find
  -- all morphisms \(h : B \to C\), of the given class, such that \(g = h \circ f\).
  --
  -- @
  --      f    
  --   A ────▶ B
  --    ╲      │
  --   g ╲     │ h
  --      ╲    ▼
  --       ╲─▶ C
  -- @
  findSpanCommuters :: MorphismClass cat -> morph -> morph -> cat [morph]

  -- | Given a cospan \(B \overset{f}{\to} A \overset{g}{\leftarrow} C\), find
  -- all morphisms \(h : B \to C\), of the given class, such that \(f = g \circ h\).
  --
  -- @
  --       f
  --    B ────▶ A
  --    │       ▲
  --  h │      ╱ 
  --    ▼     ╱ g
  --    C ───╱
  -- @
  findCospanCommuters :: MorphismClass cat -> morph -> morph -> cat [morph]

  -- | Given two lists of TypedGraphMorphism @fi : Ai -> B@ and @gi : Ai -> C@ it induces a Morphism
  -- @h : B -> C@ shuch that @h . fi = gi@ for all @i@. The lists must have the same length and must
  -- not be empty.
  --
  -- __WARNING:__ since such a morphism may not exist, this may return an invalid morphism.
  induceSpanMorphism :: [morph] -> [morph] -> cat morph

-- | Type class for categories that have all finite limits.
--
-- This type class provides constructions for initial objects, equalizers,
-- products and pullbacks. With these, it should be possible (if not efficient)
-- to construct any finite limit.
--
-- Even though pullbacks could be 
-- calculated by a product followed by an equalizer, no default implementation for them is provided. This is because product
-- objects tend to be very large, so the naive implementation of pullbacks would
-- be inefficient.
--
-- Since equalizers, products and pullbacks are associative, the default
-- implementation for their n-ary versions consists of repeated application of
-- the binary versions. Specialized implementations might reduce execution time
-- and memory consumption.
--
-- If the category has a final object, then products can be calculated as
-- pullbacks of the morphisms to the final object. Thus, there is a default
-- implementation of products for 'Cocomplete' categories, in terms of the final
-- object and pullbacks.
class Category cat morph => Complete cat morph where

  -- | Obtain the initial object of the category.
  --
  -- An object \(\mathbf{0}\) is initial when, for any other object \(X\), there
  -- is a unique morphism \(\mathbf{0} \to X\). Note that there isn't
  -- necessarily a unique initial object, but all of them are isomorphic. This
  -- function always returns the same initial object.
  getInitialObject :: cat (Obj cat)

  -- | Obtain the unique morphism from the initial object into the given object.
  --
  -- It is guaranteed that the domain of this morphism is /equal/ to the initial
  -- object returned by 'getInitialObject', and not just isomorphic.
  getMorphismFromInitialObjectTo :: Obj cat -> cat morph

  -- | Given two parallel morphisms, calculate their equalizer.
  --
  -- Given two morphisms \(f,g : X \to Y\), returns their equalizer \(p : E \to X\).
  calculateEqualizer :: morph -> morph -> cat morph

  -- | Given any number of parallel morphisms, calculate their equalizer.
  --
  -- Given a non-empty family of parallel morphisms \(\{f_i : X \to Y\}\),
  -- returns their equalizer \(p : E \to X\).
  calculateNEqualizer :: NonEmpty morph -> cat morph
  calculateNEqualizer (f :| [])  = return $ identity (domain f)
  calculateNEqualizer (f :| [g]) = calculateEqualizer f g
  calculateNEqualizer (f :| g : gs) = do
    q <- calculateNEqualizer (g :| gs)
    p <- calculateEqualizer (f <&> q) (g <&> q)
    return (q <&> p)

  -- | Given two objects, calculate their product.
  --
  -- Given two objects \(X,Y\), returns the pair of projection morphisms
  -- \((p_X, p_Y)\) from the product
  -- \(X \overset{p_X}{\leftarrow} X\times Y \overset{p_Y}{\to} Y\).
  calculateProduct :: Obj cat -> Obj cat -> cat (morph, morph)
  default calculateProduct :: Cocomplete cat morph => Obj cat -> Obj cat -> cat (morph, morph)
  calculateProduct x y = do
    fx <- getMorphismToFinalObjectFrom x
    fy <- getMorphismToFinalObjectFrom y
    (py, px) <- calculatePullback fx fy
    return (px, py)

  -- | Given any number of objects, calculate their product.
  --
  -- Given a family of objects \(\{X_i\}\), return the pair
  -- \((P, \{p_i : P \to X_i\})\), where \(P\) is the product object
  -- of all \(X_i\) and each \(p_i\) is a projection morphism.
  calculateNProduct :: [Obj cat] -> cat (Obj cat, [morph])
  calculateNProduct [] = (,[]) <$> getInitialObject
  calculateNProduct [x] = return (x, [identity x])
  calculateNProduct xs = do
    ps <- recurse xs
    return (domain $ head ps, ps)
    where
      recurse [] = error "unreachable"
      recurse [_] = error "unreachable"
      recurse [x,y] = do
        (px, py) <- calculateProduct x y
        return [px, py]
      recurse (x:ys) = do
        qs <- recurse ys
        (px, pys) <- calculateProduct x (domain $ head qs)
        return (px : map (<&>pys) qs)

  -- | Given two morphisms with the same codomain, calculate their pullback.
  --
  -- Given two morphisms \(f, g\) with same codomain, return the pair of
  -- morphisms \((f', g')\) making the following square a pullback.
  --
  -- @
  --        g'
  --     P──────▶X
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     Y──────▶Z
  --        g
  -- @
  calculatePullback :: morph -> morph -> cat (morph, morph)

  -- | Given any number of morphisms with the same codomain, calculate their pullback.
  --
  -- Given a family of morphisms with same codomain \(\{f_i : X_i \to Z\}\),
  -- return the pair \((P, \{p_i : P \to X_i\})\) where \(P\) is the pullback
  -- object for all \(f_i\) and each \(p_i\) is a projection morphism.
  calculateNPullback :: [morph] -> cat (Obj cat, [morph])
  calculateNPullback [] = (,[]) <$> getInitialObject
  calculateNPullback [f] = return (domain f, [identity $ domain f])
  calculateNPullback fs = do
    ps <- recurse fs
    return (domain (head ps), ps)
    where
      recurse [] = error "unreachable"
      recurse [_] = error "unreachable"
      recurse [f, g] = do
        (pg, pf) <- calculatePullback f g
        return [pf, pg]
      recurse (f : g : gs) = do
        qs <- recurse (g : gs)
        (pf, pgs) <- calculatePullback f (g <&> head qs)
        return (pf : map (<&>pgs) qs)


-- | Type class for categories that have all finite colimits.
--
-- This type class provides constructions for final objects, coequalizers,
-- coproducts and pushouts. With these, it should be possible (if not efficient)
-- to construct any finite colimit.
--
-- Since pushouts can be calculated by a coproduct followed by a coequalizer,
-- their implementation may be omitted. Since coproduct objects are not usually
-- too large, using the default implementation of pushouts is often acceptable
-- in terms of execution times and memory consumption.
--
-- Since coequalizers, coproducts and pushouts are associative, the default
-- implementation for their n-ary versions consists of repeated application of
-- the binary versions. Specialized implementations might reduce execution time
-- and memory consumption.
class Category cat morph => Cocomplete cat morph where
  {-# MINIMAL getFinalObject, getMorphismToFinalObjectFrom, calculateCoequalizer, calculateCoproduct #-}

  -- | Obtain the final object of the category.
  --
  -- An object \(\mathbf{1}\) is final when, for any other object \(X\), there
  -- is a unique morphism \(X \to \mathbf{1}\). Note that there isn't
  -- necessarily a unique final object, but all of them are isomorphic. This
  -- function always returns the same final object.
  getFinalObject :: cat (Obj cat)

  -- | Obtain the unique morphism from the given object into the final object.
  --
  -- It is guaranteed that the codomain of this morphism is /equal/ to the final
  -- object returned by 'getFinalObject', and not just isomorphic.
  getMorphismToFinalObjectFrom :: Obj cat -> cat morph

  -- | Given two parallel morphisms, calculate their coequalizer.
  --
  -- Given two morphisms \(f,g : X \to Y\), returns their equalizer \(j : Y \to E\).
  calculateCoequalizer :: morph -> morph -> cat morph

  -- | Given any number of parallel morphisms, calculate their coequalizer.
  --
  -- Given a non-empty family of parallel morphisms \(\{f_i : X \to Y\}\),
  -- returns their equalizer \(j : Y \to E\).
  calculateNCoequalizer :: NonEmpty morph -> cat morph
  calculateNCoequalizer (f :| [])  = return $ identity (codomain f)
  calculateNCoequalizer (f :| [g]) = calculateCoequalizer f g
  calculateNCoequalizer (f :| g : gs) = do
    k <- calculateNCoequalizer (g :| gs)
    j <- calculateCoequalizer (k <&> f) (k <&> g)
    return (j <&> k)

  -- | Given two objects, calculate their coproduct.
  --
  -- Given two objects \(X,Y\), returns the pair of injection morphisms
  -- \((j_X, j_Y)\) from the coproduct
  -- \(X \overset{j_X}{\to} X+Y \overset{j_Y}{\leftarrow} Y\).
  calculateCoproduct :: Obj cat -> Obj cat -> cat (morph, morph)

  -- | Given any number of objects, calculate their coproduct.
  --
  -- Given a family of objects \(\{X_i\}\), return the pair
  -- \((S, \{j_i : X_i \to S\})\), where \(S\) is the product object
  -- of all \(X_i\) and each \(j_i\) is an injection morphism.
  calculateNCoproduct :: [Obj cat] -> cat (Obj cat, [morph])
  calculateNCoproduct [] = (,[]) <$> getFinalObject
  calculateNCoproduct [x] = return (x, [identity x])
  calculateNCoproduct xs = do
    ps <- recurse xs
    return (domain $ head ps, ps)
    where
      recurse [] = error "unreachable"
      recurse [_] = error "unreachable"
      recurse [x,y] = do
        (jx, jy) <- calculateCoproduct x y
        return [jx, jy]
      recurse (x:ys) = do
        ks <- recurse ys
        (jx, jys) <- calculateCoproduct x (codomain $ head ks)
        return (jx : map (jys<&>) ks)

  -- | Given two morphisms with same domain, calculate their pushout.
  --
  -- Given two morphisms \(f, g\) with same domain, return the pair of
  -- morphisms \((f', g')\) making the following square a pushout.
  --
  -- @
  --        f
  --     Z──────▶X
  --     │       │
  --   g │       │ g'
  --     ▼       ▼
  --     Y──────▶S
  --        f'
  -- @
  calculatePushout :: morph -> morph -> cat (morph, morph)
  calculatePushout f g = do
    let (x, y) = (codomain f, codomain g)
    (jx, jy) <- calculateCoproduct x y
    k <- calculateCoequalizer (jx <&> f) (jy <&> g)
    return (k <&> jy, k <&> jx)

  -- | Given any number of morphisms with same domain, calculate their pushout.
  --
  -- Given a family of morphisms with same domain \(\{f_i : Z \to X_i\}\),
  -- return the pair \((S, \{j_i : X_i \to S\})\) where \(S\) is the pushout
  -- object for all \(f_i\) and each \(j_i\) is an injection morphism.
  calculateNPushout :: [morph] -> cat (Obj cat, [morph])
  calculateNPushout [] = (,[]) <$> getFinalObject
  calculateNPushout [f] = return (codomain f, [identity $ codomain f])
  calculateNPushout fs = do
    js <- recurse fs
    return (codomain (head js), js)
    where
      recurse [] = error "unreachable"
      recurse [_] = error "unreachable"
      recurse [f, g] = do
        (jy, jx) <- calculatePushout f g
        return [jx, jy]
      recurse (f : g : gs) = do
        ks <- recurse (g : gs)
        (jf, jgs) <- calculatePushout f (g <&> head ks)
        return (jf : map (jgs<&>) ks)


class Category cat morph => Adhesive cat morph where
  -- | Calculate the pushout of a monomorphism and another morphism.
  --
  -- Given the morphisms \(m : Z \to X\) and \(f : Z \to Y\), respectively and
  -- with \(m\) monic, returns the pair of morphisms \(f' : Y \to S\)
  -- and \(m': X \to S\) such that the following square is a pushout.
  --
  -- @
  --       f
  --    Z──────▶Y
  --    │       │
  --  m │       │ m'
  --    ▼       ▼
  --    X──────▶S
  --       f'
  -- @
  --
  -- The behaviour of this function is undefined if the first morphism isn't
  -- monic.
  calculatePushoutAlongMono :: morph -> morph -> cat (morph, morph)
  default calculatePushoutAlongMono :: Cocomplete cat morph => morph -> morph -> cat (morph, morph)
  calculatePushoutAlongMono = calculatePushout

  -- | Calculate the pullback of a rule-morphism and another morphism.
  --
  -- Given the morphisms \(f : A \to B\) and \(g : A \to C\), respectively,
  -- returns the pair of morphisms \(f' : P \to B\) and \(g': P \to A\) such
  -- that the following square is a pullback.
  --
  -- @
  --        g'
  --     P──────▶B
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     A──────▶C
  --        g
  -- @
  --
  -- The behaviour of this function is undefined if the first morphism isn't
  -- monic.
  calculatePullback' :: morph -> morph -> cat (morph, morph)
  default calculatePullback' :: Complete cat morph => morph -> morph -> cat (morph, morph)
  calculatePullback' = calculatePullback

  -- | Check if the morphisms \(f : X \to Y\) and \(g : Y \to Z\), respectively
  -- and with at least one of them monic, have a pushout complement (see
  -- 'calculatePushoutComplementAlongMono').
  --
  -- The behaviour of this function is undefined none of the morphisms is monic.
  hasPushoutComplementAlongMono :: morph -> morph -> cat Bool

  -- | Calculate the pushout complement for a sequence of a morphisms,
  -- __assuming at least one of them is monic and the complement exists__. In
  -- order to test if the pushout complement exists, use
  -- 'hasPushoutComplementAlongMono'.
  --
  -- Given the morphisms \(f : X \to Y\) and \(g : Y \to Z\), respectively and
  -- with at least one of them monic, returns the pair of morphisms \(g' : X \to W\)
  -- and \(f' : W \to Z\) such that the following square is a pushout. Since
  -- the category is adhesive, such a pair is unique.
  --
  -- @
  --        f
  --     A──────▶B
  --     │       │
  --  g' │       │ g
  --     ▼       ▼
  --     X──────▶C
  --        f'
  -- @
  --
  -- The behaviour of this function is undefined if none of the morphisms are
  -- monic, or if the pushout complement doesn't exist.
  calculatePushoutComplementAlongMono :: morph -> morph -> cat (morph, morph)


class Category cat morph => LRNAdhesive cat morph where

  ruleMorphism :: MorphismClass cat
  leftHandMorphism :: MorphismClass cat
  matchMorphism :: MorphismClass cat

  
  -- | Calculate the pushout of a rule-morphism and a match-morphism.
  --
  -- Given the morphisms \(r : A \to B\) and \(m : A \to C\), respectively and
  -- with \(r \in \mathcal{R}\) and \(m \in \mathcal{N}\), returns the pair of
  -- morphisms \(r' : C \to D\) and \(m': B \to D\) such that the following
  -- square is a pushout.
  --
  -- @
  --       r
  --    A──────▶B
  --    │       │
  --  m │       │ m'
  --    ▼       ▼
  --    C──────▶D
  --       r'
  -- @
  --
  -- The behaviour of this function is undefined if the morphisms don't belong
  -- to the appropriate classes.
  calculatePushoutAlongRN :: morph -> morph -> cat (morph, morph)
  default calculatePushoutAlongRN :: Cocomplete cat morph => morph -> morph -> cat (morph, morph)
  calculatePushoutAlongRN = calculatePushout

  -- | Calculate the pullback of a rule-morphism and another morphism.
  --
  -- Given the morphisms \(r : A \to B\) and \(f : A \to C\), respectively and
  -- with \(r \in \mathcal{R}\), returns the pair of morphisms \(r' : X \to B\)
  -- and \(f': X \to A\) such that the following square is a pullback.
  --
  -- @
  --        r'
  --     X──────▶B
  --     │       │
  --  f' │       │ f
  --     ▼       ▼
  --     A──────▶C
  --        r
  -- @
  --
  -- The behaviour of this function is undefined if the morphisms don't belong
  -- to the appropriate classes.
  calculatePullbackAlongR :: morph -> morph -> cat (morph, morph)
  default calculatePullbackAlongR :: Complete cat morph => morph -> morph -> cat (morph, morph)
  calculatePullbackAlongR = calculatePullback

  -- | Check if the morphisms \(r : A \to B\) and \(m : B \to C \), respectively
  -- and with \(r \in \mathcal{R}\) and \(m \in \mathcal{N}\), have a pushout
  -- complement (see 'calculatePushoutComplementOfRN').
  --
  -- The behaviour of this function is undefined if the morphisms don't belong
  -- to the appropriate classes.
  hasPushoutComplementOfRN :: morph -> morph -> cat Bool

  -- | Calculate the pushout complement for a sequence of a rule-morphism and a
  -- match-morphism, __assuming it exists__. In order to test if the pushout
  -- complement exists, use 'hasPushoutComplementOfRN'.
  --
  -- Given the morphisms \(r : A \to B\) and \(m : B \to C\), respectively and
  -- with \(r \in \mathcal{R}\) and \(m \in \mathcal{N}\), returns the pair of
  -- morphisms \(m' : A \to X\) and \(r' : X \to C\) such that the following
  -- square is a pushout. Since the category is Adhesive, such a pair is unique.
  --
  -- @
  --        r
  --     A──────▶B
  --     │       │
  --  m' │       │ m
  --     ▼       ▼
  --     X──────▶C
  --        r'
  -- @
  --
  -- The behaviour of this function is undefined if the morphisms don't belong
  -- to the appropriate classes.
  calculatePushoutComplementOfRN :: morph -> morph -> cat (morph, morph)

isRuleMorphism :: forall cat morph. LRNAdhesive cat morph => morph -> cat Bool
isRuleMorphism = (`belongsToClass` ruleMorphism @cat)

isLeftHandMorphism :: forall cat morph. LRNAdhesive cat morph => morph -> cat Bool
isLeftHandMorphism = (`belongsToClass` leftHandMorphism @cat)

isMatchMorphism :: forall cat morph. LRNAdhesive cat morph => morph -> cat Bool
isMatchMorphism = (`belongsToClass` matchMorphism @cat)

class MFinitary cat morph => InitialPushout cat morph where
  -- | Calculate the \(\mathcal{M}\)-initial pushout of the given morphism.
  --
  -- Given the morphism \(f : A \to A'\), returns the morphisms \(b : B \to A\),
  -- \(f' : B \to C\) and \(c: C \to A'\), respectively and with
  -- \(b,c \in \mathcal{M}\), such that the following square is an
  -- \(\mathcal{M}\)-initial pushout of \(f\).
  --
  -- @
  --        f'
  --    B──────▶C
  --    │       │
  --  b │       │ c
  --    ▼       ▼
  --    A──────▶A'
  --        f
  -- @
  calculateInitialPushout :: morph -> cat (morph, morph, morph)

