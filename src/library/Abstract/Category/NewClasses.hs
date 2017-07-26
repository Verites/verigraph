{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
module Abstract.Category.NewClasses where

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
  type MorphismClass cat :: *
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


class Category cat morph => MFinitary cat morph where

  -- | A class of morphisms whose equivalence classes define subobjects of their
  -- codomains. Usually denoted by \(\mathcal{M}\). Must be a subclass of 'monic'.
  subobject :: MorphismClass cat

  -- | Construct a canonical version of the subobject. The behaviour is
  -- undefined when the given morphism is not in the class of 'subobject's.
  getCanonicalSubobject :: morph -> cat morph

  -- | Obtain the finite list of all subobjects of a given object.
  allSubobjectsOf :: Obj cat -> cat [morph]

isSubobject :: forall cat morph. MFinitary cat morph => morph -> cat Bool
isSubobject = (`belongsToClass` subobject @cat)


class Category cat morph => ECofinitary cat morph where

  quotient :: MorphismClass cat

  getCanonicalQuotient :: morph -> cat morph

  allQuotientsOf :: Obj cat -> cat [morph]

isQuotient :: forall cat morph. ECofinitary cat morph => morph -> cat Bool
isQuotient = (`belongsToClass` quotient @cat)

class ECofinitary cat morph => EM'Factorizable cat morph where

  monicFactor :: MorphismClass cat

  factorize :: morph -> cat (morph, morph)

isMonicFactor :: forall cat morph. EM'Factorizable cat morph => morph -> cat Bool
isMonicFactor = (`belongsToClass` monicFactor @cat)


class Category cat morph => FindMorphism cat morph where
  findMorphisms :: MorphismClass cat -> Obj cat -> Obj cat -> cat [morph]

  findCospanCommuters :: MorphismClass cat -> morph -> morph -> cat [morph]

  findSpanCommuters :: MorphismClass cat -> morph -> morph -> cat [morph]

  -- | Given two lists of TypedGraphMorphism @fi : Ai -> B@ and @gi : Ai -> C@ it induces a Morphism
  -- @h : B -> C@ shuch that @h . fi = gi@ for all @i@. The lists must have the same length and must
  -- not be empty.
  --
  -- __WARNING:__ since such a morphism may not exist, this may return an invalid morphism.
  induceSpanMorphism :: [morph] -> [morph] -> cat morph

class Category cat morph => Complete cat morph where
  getInitialObject :: cat (Obj cat)
  getMorphismFromInitialObjectTo :: Obj cat -> cat morph

  calculateEqualizer :: morph -> morph -> cat morph
  calculateNEqualizer :: [morph] -> cat morph

  calculateProduct :: Obj cat -> Obj cat -> cat (morph, morph)
  calculateNProduct :: [Obj cat] -> cat [morph]

  calculatePullback :: morph -> morph -> cat (morph, morph)
  calculateNPullback :: [morph] -> cat [morph]


class Category cat morph => Cocomplete cat morph where
  getFinalObject :: cat (Obj cat)
  getMorphismFromFinalObjectTo :: Obj cat -> cat morph

  calculateCoequalizer :: morph -> morph -> cat morph
  calculateNCoequalizer :: [morph] -> cat morph

  calculateCoproduct :: Obj cat -> Obj cat -> cat (morph, morph)
  calculateNCoproduct :: [Obj cat] -> cat [morph]

  calculatePushout :: morph -> morph -> cat morph
  calculateNPushout :: [morph] -> cat [morph]

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

class Category cat morph => LRNAdhesive cat morph where

  ruleMorphism :: MorphismClass cat
  leftHandMorphism :: MorphismClass cat
  matchMorphism :: MorphismClass cat

  calculatePushoutAlongRN :: morph -> morph -> cat (morph, morph)

  calculatePullbackAlongR :: morph -> morph -> cat (morph, morph)

  hasPushoutComplementOfRN :: morph -> morph -> cat Bool

  calculatePushoutComplementOfRN :: morph -> morph -> cat (morph, morph)

isRuleMorphism :: forall cat morph. LRNAdhesive cat morph => morph -> cat Bool
isRuleMorphism = (`belongsToClass` ruleMorphism @cat)

isLeftHandMorphism :: forall cat morph. LRNAdhesive cat morph => morph -> cat Bool
isLeftHandMorphism = (`belongsToClass` leftHandMorphism @cat)

isMatchMorphism :: forall cat morph. LRNAdhesive cat morph => morph -> cat Bool
isMatchMorphism = (`belongsToClass` matchMorphism @cat)

class MFinitary cat morph => InitialPushout cat morph where
  initialPushout :: morph -> cat (morph, morph, morph)

