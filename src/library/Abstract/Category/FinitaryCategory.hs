{-# LANGUAGE TypeFamilies #-}
module Abstract.Category.FinitaryCategory where

{- | Defines finitary categories and the basic operations they have-}
class (Eq morph) => FinitaryCategory morph where
    {- | Defines the object of the category in terms of its morphism -}
    type Obj morph :: *
    {- | Morphism composition operator. Given two morphisms @f:A─>B@, and @g:B─>C@, it composes the morphisms returnig a
          morphism @h:A->C@. The order of arguments is the same as in function composition
          i.e.: @g \<&\> f@ = @g . f@ -}
    (<&>)    :: morph -> morph -> morph
    {- | Given an object @A@, it returns an identity morphism @id:A─>A@ -}
    identity :: Obj morph -> morph
    {- | Given a morphism @f:A─>B@ it returns the object @A@-}
    domain   :: morph -> Obj morph
    {- | Given a morphism @f:A─>B@ it returns the object @B@-}
    codomain :: morph -> Obj morph

    isMonomorphism :: morph -> Bool
    isEpimorphism  :: morph -> Bool
    isIsomorphism  :: morph -> Bool

-- | Enum for the types of morphisms that can be used / found
data MorphismType
  = GenericMorphism
  | Monomorphism
  | Epimorphism
  | Isomorphism
  deriving (Show, Enum, Eq)

type Span morph = (morph, morph)


class FinitaryCategory morph => FindMorphism morph where
  -- | Given a type __/t/__ of @MorphismType@ and two objects __/A/__ and __/B/__, it finds all the matches
  -- m : A -> B in which morph is of the type t
  findMorphisms :: MorphismType -> Obj morph -> Obj morph -> [morph]

  -- | Given two lists of TypedGraphMorphism @fi : Ai -> B@ and @gi : Ai -> C@ it induces a Morphism
  -- @h : B -> C@ shuch that @h . fi = gi@ for all @i@. The lists must have the same length and must
  -- not be empty.
  induceSpanMorphism :: [morph] -> [morph] -> morph

  -- TODO: replace by data constructor @PartMono :: m -> MorphismType@?
  -- | Given a NAC /n : L -> N/ and a match /m : L -> G/, it finds all
  -- morphisms /q : N -> G/ that are partial injective.
  --
  -- A partial injective restriction demands the /q : N -> G/ to be
  -- non-injective only on /N \\ n(L)/.
  partialInjectiveMatches :: morph -> morph -> [morph]

  -- | Given two TypedGraphMorphism @f : B -> A@ and @g : C -> A@ it finds a list of Morphisms
  -- @hi : B -> C@ shuch that @f . ¬g  = hi@ for all @i@.
  findCospanCommuter :: MorphismType -> morph -> morph -> [morph]


-- | Given two objects A and B, finds all monomorphisms from A to B
findMonomorphisms :: FindMorphism morph => Obj morph -> Obj morph -> [morph]
findMonomorphisms = findMorphisms Monomorphism

-- | Given two objects A and B, finds all epimorphisms from A to B
findEpimorphisms :: FindMorphism morph => Obj morph -> Obj morph -> [morph]
findEpimorphisms = findMorphisms Epimorphism

-- | Given two objects A and B, finds all isomorphisms from A to B
findIsomorphisms :: FindMorphism morph => Obj morph -> Obj morph -> [morph]
findIsomorphisms = findMorphisms Isomorphism

-- | Given two objects A and B, finds all morphisms from A to B
findAllMorphisms :: FindMorphism morph => Obj morph -> Obj morph -> [morph]
findAllMorphisms = findMorphisms GenericMorphism
