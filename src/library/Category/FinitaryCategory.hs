{-# LANGUAGE TypeFamilies #-}
module Category.FinitaryCategory where

class (Eq morph) => FinitaryCategory morph where
    type Obj morph :: *
    -- | Apply the first argument first (compose f g = g . f)
    compose  :: morph -> morph -> morph
    domain   :: morph -> Obj morph
    codomain :: morph -> Obj morph
    identity :: Obj morph -> morph
    isMonomorphism :: morph -> Bool
    isEpimorphism  :: morph -> Bool
    isIsomorphism  :: morph -> Bool

-- | Enum for the types of morphisms that can be used / found
data MorphismType
  = GenericMorphism
  | Monomorphism
  | Epimorphism
  | Isomorphism
  deriving (Show, Enum)

type Span morph = (morph, morph)


class FinitaryCategory morph => FindMorphism morph where
  -- | Given a type __/t/__ of @MorphismType@ and two objects __/A/__ and __/B/__, it finds all the matches
  -- m : A -> B in which morph is of the type t
  findMorphisms :: MorphismType -> Obj morph -> Obj morph -> [morph]

  -- | Given two objects A and B, finds all monomorphisms from A to B
  findMonomorphisms :: Obj morph -> Obj morph -> [morph]
  findMonomorphisms = findMorphisms Monomorphism

  -- | Given two objects A and B, finds all epimorphisms from A to B
  findEpimorphisms :: Obj morph -> Obj morph -> [morph]
  findEpimorphisms = findMorphisms Epimorphism

  -- | Given two objects A and B, finds all isomorphisms from A to B
  findIsomorphisms :: Obj morph -> Obj morph -> [morph]
  findIsomorphisms = findMorphisms Isomorphism

  -- | Given two objects A and B, finds all morphisms from A to B
  findAllMorphisms :: Obj morph -> Obj morph -> [morph]
  findAllMorphisms = findMorphisms GenericMorphism

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
