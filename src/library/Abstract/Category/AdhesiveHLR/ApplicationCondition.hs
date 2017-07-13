module Abstract.Category.AdhesiveHLR.ApplicationCondition (

) where

import           Abstract.Category.AdhesiveHLR hiding (Constraint(..), AtomicConstraint(..))
import           Base.Valid

data AtomicApplicationCondition morph = AtomicApplicationCondition {
   name       :: String
,  positive :: Bool
,  base       :: morph
,  extensions :: [morph]
} deriving (Show)

instance Valid morph => Valid (AtomicApplicationCondition morph) where
  validate (AtomicApplicationCondition n _ b es) =
    mconcat $
      [withContext ("base morphism of " ++ n)  (validate b)]
       ++ zipWith validateExtension es ([0..] :: [Int])
    where
      validateExtension e index =
        mconcat
          [ withContext
            ("extension morphism #" ++ show index ++ " of atomic application condition " ++ n)
            (validate e) ]

satisfiesAtomicApplicationCondition :: (AdhesiveHLR morph) => morph
  -> AtomicApplicationCondition morph -> Bool
satisfiesAtomicApplicationCondition m condition = Prelude.null ps || allPsAreSatisfied
  where
    object = codomain m
    x = base condition
    xis = extensions condition
    psCandidates = findMMorphisms (codomain x) object
    ps = filter (\p -> p <&> x == m) psCandidates
    positiveSatisfaction = all (\p ->       existsCommutativeQ p xis) ps
    negativeSatisfaction = all (\p -> not $ existsCommutativeQ p xis) ps
    allPsAreSatisfied = if positive condition then positiveSatisfaction else negativeSatisfaction

existsCommutativeQ :: (AdhesiveHLR morph) => morph -> [morph] -> Bool
existsCommutativeQ p = any satisfies
  where
    object = codomain p
    qs xi = findMMorphisms (codomain xi) object
    satisfies xi = any (\q -> q <&> xi == p) (qs xi)

data ApplicationCondition morph =
    Atomic { atomic :: AtomicApplicationCondition morph }
  | And { lc :: ApplicationCondition morph,
          rc :: ApplicationCondition morph}
  | Or{ lc :: ApplicationCondition morph,
        rc :: ApplicationCondition morph}
  | Not { nc :: ApplicationCondition morph }
  deriving (Show)

satisfiesApplicationCondition :: (AdhesiveHLR morph) => morph -> ApplicationCondition morph -> Bool
satisfiesApplicationCondition m condition =
  case condition of
    Atomic atomic -> satisfiesAtomicApplicationCondition m atomic
    Not nc        -> not $ satisfiesApplicationCondition m nc
    And lc rc     -> satisfiesApplicationCondition m lc && satisfiesApplicationCondition m rc
    Or lc rc      -> satisfiesApplicationCondition m lc || satisfiesApplicationCondition m rc

satisfiesAllApplicationConditions :: (AdhesiveHLR morph) => morph -> [ApplicationCondition morph] -> Bool
satisfiesAllApplicationConditions m = all (satisfiesApplicationCondition m)
