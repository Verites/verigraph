{-|
Module      : DiagramAlgorithms
Description : Implements diagram-based algorithms

This diagram shows objects and morphisms names used in the algorithms below:

 @
                     N1    N2
                     ^     ^
         r1     l1   │n1 n2│  l2     r2
     R1◀─────K1────▶L1    L2◀────K2─────▶R2
     │       │       \\   /       │       │
  m1'│     k1│      m1\\ /m2      │       │
     ▼       ▼          ▼         ▼       ▼
     P1◀─────D1───────▶G◀───────D2──────▶P2
         e1       d1
 @

q21 : N2 -> P1
h21 : L2 -> D1
m2' : L2 -> P1

prod1 = l1 r1 {n1}

prod2 = l2 r2 {n2}
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Abstract.Rewriting.DPO.DiagramAlgorithms (
    isDeleteUse
  , isProduceDangling
  , isProduceForbid
  , isProduceUse
  , isDeleteForbid
  , deleteUseDangling
  , produceForbidOneNac
  ) where


import           Control.Applicative
import           Control.Monad.List

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import Util.Monad


-- | Rule @p1@ is in a delete-use conflict with @p2@ if @p1@ deletes
-- something that is used by @p2@.
--
-- Verifies the non existence of h21: L2 -> D1 such that d1 . h21 = m2
isDeleteUse :: forall cat morph. DPO cat morph => Production cat morph -> (morph,morph) -> cat Bool
isDeleteUse p1 (m1,m2) = do
  (_,d1) <- calculatePushoutComplementOfRN (leftMorphism p1) m1 --gets only the morphism D1 to G
  h21 <- findCospanCommuters (matchMorphism @cat) m2 d1
  return (null h21)

isProduceUse :: forall cat morph. DPO cat morph => Production cat morph -> (morph,morph) -> cat Bool
isProduceUse p1 (m1',m2) = do
  (_,e1) <- calculatePushoutComplementOfRN (rightMorphism p1) m1' --gets only the morphism D1 to G
  h21 <- findCospanCommuters (matchMorphism @cat) m2 e1
  return (null h21)

-- Runs the DPO rewriting on p1 with m1 and
-- returns h21 and h21_e1 morphism as diagram above
getM2AfterRewriting :: forall cat morph. DPO cat morph => Production cat morph -> morph -> morph -> cat (Maybe morph)
getM2AfterRewriting p1 m1 m2 = do
  (k1, d1) <- calculatePushoutComplementOfRN (leftMorphism p1) m1 
  (_, e1) <- calculatePushoutAlongRN (rightMorphism p1) k1
  h21Candidates <- findCospanCommuters (matchMorphism @cat) m2 d1
  return $ case h21Candidates of
    [] -> Nothing
    [h21] -> Just (e1 <&> h21) --h21 is unique if it exists
    _ -> error "getH21FromRewriting: non-unique h21"

-- | Rule @p1@ is in a produce-dangling conflict with @p2@ if @p1@
-- produces something that disables @p2@.
--
-- Gets the match of @p1@ from L2 to P1, checks if satisfiesNACs and not satisfiesGluingConditions
isProduceDangling :: DPO cat morph => Production cat morph -> Production cat morph -> (morph,morph) -> cat Bool
isProduceDangling p1 p2 (m1,m2) = do
  maybeM2' <- getM2AfterRewriting p1 m1 m2
  case maybeM2' of
    Nothing -> return False
    Just m2' -> (not <$> satisfiesGluingConditions p2 m2') `andM` satisfiesNACs p2 m2'

isProduceForbid :: DPO cat morph => Production cat morph -> Production cat morph -> (morph,morph) -> cat Bool
isProduceForbid p1 p2 (m1,m2) = do
  maybeM2' <- getM2AfterRewriting p1 m1 m2
  case maybeM2' of
    Nothing -> return False
    Just m2' -> satisfiesGluingConditions p2 m2' `andM` (not <$> satisfiesNACs p2 m2')

isDeleteForbid :: forall cat morph. DPO cat morph => Production cat morph -> Production cat morph -> (morph,morph) -> cat Bool
isDeleteForbid p1 p2 (m1',m2') = do
  hasRewriting <- hasPushoutComplementOfRN (rightMorphism p1) m1'
  if not hasRewriting
    then return False
    else do
      (k1, e1) <- calculatePushoutComplementOfRN (rightMorphism p1) m1'
      (_, d1) <- calculatePushoutAlongRN k1 (leftMorphism p1)
      h21Candidates <- findCospanCommuters (matchMorphism @cat) m2' e1
      let m2 = d1 <&> head h21Candidates
      (return . not $ null h21Candidates) `andM` satisfiesGluingConditions p2 m2 `andM` (not <$> satisfiesNACs p2 m2)

-- | Verifies delete-use, if false verifies produce-dangling.
-- Returns Left in the case of delete-use and Right for produce-dangling.
deleteUseDangling :: DPO cat morph => Production cat morph -> Production cat morph -> (morph,morph)-> cat (Maybe (Either (morph,morph) (morph,morph)))
deleteUseDangling p1 p2 (m1,m2) = do
  maybeM2' <- getM2AfterRewriting p1 m1 m2
  case maybeM2' of
    Nothing -> return $ Just (Left (m1,m2)) -- delete use case
    Just m2' -> do
      hasProduceDangling <- (not <$> satisfiesGluingConditions p2 m2') `andM` satisfiesNACs p2 m2'
      if hasProduceDangling 
        then return $ Just (Right (m1,m2))
        else return Nothing

-- | Rule @p1@ is in a produce-forbid conflict with @p2@ if @p1@
-- produces something that enables some nac of @p2@.
--
-- Checks produce-forbid for a NAC @n@ in @p2@
produceForbidOneNac :: forall cat morph. (DPO cat morph, EM'PairFactorizable cat morph) => Production cat morph
                    -> Production cat morph -> (morph, Int) -> cat [((morph,morph), (morph,morph), (morph,Int))]
produceForbidOneNac p1 p2 (n2,idx) = runListT $ do
  let p1' = invertProduction p1

  -- Pick a jointly epi pair /R1 -m1'-> P1 <-q21- N2/
  (m1', q21) <- pickOne $ findJointlyEpicPairs (matchMorphism @cat, rightObject p1) (monic @cat, codomain n2)

  -- Reconstruct the match /m1/ that would lead to this pair
  guardM (satisfiesRewritingConditions p1' m1')
  (k1, m1, _, d1) <- lift $ calculateDPO m1' p1'
  guardM (satisfiesNACs p1 m1)

  -- Look for morphisms /h21 : L2 -> D1/
  let m2' = q21 <&> n2
  h21Candidates <- lift $ findCospanCommuters (matchMorphism @cat) m2' k1
  case h21Candidates of
    [] ->
      -- No proper h21, so no produce-forbid conflict
      empty

    [h21] -> do
      let m2 = d1 <&> h21
      guardM (satisfiesRewritingConditions p2 m2)

      -- There is h21 and the match m2 is valid, so there is a conflict
      return ((m1, m2), (m1', m2'), (q21, idx))

    _ -> error "produceForbidOneNac: h21 should be unique, but isn't"