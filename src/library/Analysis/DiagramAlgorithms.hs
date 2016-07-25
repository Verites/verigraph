{-|
Module      : DiagramAlgorithms
Description : Implements some diagram-based algorithms
Stability   : stable

This diagram shows objects and morphisms names used in the algorithms below:

 @
                    N1    N2
                    ^      ^
         r1     l1  │n1  n2│  l2     r2
     R1◀─────K1────▶L1    L2◀────K2─────▶R2
     │       │       \\   /       │       │
  m1'│      k│      m1\\ /m2      │       │
     ▼       ▼         ▼         ▼       ▼
     P1◀─────D1───────▶G◀───────D2──────▶P2
         r'       l'
 @

p1 = l1 r1 {n1}

p2 = l2 r2 {n2}
-}

module Analysis.DiagramAlgorithms (
    deleteUse
  , produceDangling
  , deleteUseDangling
  , produceForbidOneNac
  ) where

import           Abstract.Morphism
import           Data.List                 (elemIndex)
import           Abstract.AdhesiveHLR      as RW
import           Abstract.DPO              as RW hiding (comatch)

-- | This diagram shows graphs and morphisms names used in the algorithms below
--
-- @
--                    N1    N2
--                    ^      ^
--          r     l   │      │n
--     R1◀─────K1────▶L1    L2◀────K2─────▶R2
--     │       │       \\   /       │       │
--  m1'│      k│      m1\\ /m2      │       │
--     ▼       ▼         ▼         ▼       ▼
--     P1◀─────D1───────▶G◀───────D2──────▶P2
--         r'       l'
-- @
--

-- | Rule @p1@ is in a delete-use conflict with @p2@ if @p1@ deletes
-- something that is used by @p2@.
--
-- Verifies the non existence of h21: L2 -> D1 such that l' . h21 = m2
deleteUse :: DPO m => DPOConfig -> Production m -> (m, m) -> Bool
deleteUse config l (m1,m2) = null matchD
    where
        (_,l') = RW.pushoutComplement m1 (left l) --get only the morphism D2 to G
        restriction = matchRestrictionToProp (matchRestriction config)
        l2TOd1 = findMorphisms restriction (domain m2) (domain l')
        matchD = filter (\x -> m2 == compose x l') l2TOd1

-- | Rule @p1@ is in a produce-dangling conflict with @p2@ if @p1@
-- produces something that unable @p2@.
--
-- Gets the match of @p1@ from L2 to P1, checks if satsNacs and not satsGluing
produceDangling :: DPO m => DPOConfig -> Production m -> Production m -> (m, m) -> Bool
produceDangling config l r (m1,m2) = not (null matchD) && not (satsGluing config (left r) m2') && satsNacs config r m2'
    where
      (k,l') = RW.pushoutComplement m1 (left l)
      morphismRestriction = matchRestrictionToProp (matchRestriction config)
      l2TOd1 = findMorphisms morphismRestriction (domain m2) (domain l')
      matchD = filter (\x -> m2 == compose x l') l2TOd1
      (_,r') = RW.pushout k (right l)
      m2' = if length matchD > 1
              then error "produceDangling: non unique h21 morphism"
              else compose (head matchD) r' --matchD is unique if exists

-- | Verifies delete-use, if false verifies produce-dangling.
-- Returns Left in the case o delete-use and Right for produce-dangling.
deleteUseDangling :: DPO m => DPOConfig -> Production m
                  -> Production m -> (m, m)-> Maybe (Either (m,m) (m,m))
deleteUseDangling config l r (m1,m2) =
  case (null matchD, dang) of
    (True,_)     -> Just (Left (m1,m2))
    (False,True) -> Just (Right (m1,m2))
    _            -> Nothing
  where
    (k,l') = RW.pushoutComplement m1 (left l)
    morphismRestriction = matchRestrictionToProp (matchRestriction config)
    lTOd = findMorphisms morphismRestriction (domain m2) (domain l')
    matchD = filter (\x -> m2 == compose x l') lTOd
    (_,r') = RW.pushout k (right l)
    m2' = compose (head matchD) r'
    dang = not (satsGluing config (left r) m2') && satsNacs config r m2'

-- | Rule @p1@ is in a produce-forbid conflict with @p2@ if @p1@
-- produces something that able some nac of @p2@.
--
-- Checks produce-forbid for a NAC @n@ in @p2@
produceForbidOneNac :: (EpiPairs m, DPO m) => DPOConfig
                    -> Production m -> Production m -> Production m
                    -> (m, Int) -> [((m,m), (m,m), (m,Int))]
produceForbidOneNac config l inverseLeft r (n,idx) = let
        -- Consider for a NAC n (L2 -> N2) of r any jointly surjective
        -- pair of morphisms (h1: R1 -> P1, q21: N2 -> P1) with q21 (part)inj
        pairs = createPairsNac config (codomain (right l)) n

        -- Check gluing cond for (h1,r1). Construct PO complement D1.
        -- Construct PO K and abort if m1 not sats NACs l
        filtPairs = filter (\(h1,_) -> satsGluingAndNacs config inverseLeft h1) pairs

        dpo = map (\(h1,q21) ->
                    let (k,r') = RW.pushoutComplement h1 (right l)
                        (m1,l') = RW.pushout k (left l)
                    in  (h1,q21,k,r',m1,l'))
                  filtPairs --(h1,q21,k,r',m1,l')

        filtM1 = filter (\(_,_,_,_,m1,_) -> satsNacs config l m1) dpo

        --  Check existence of h21: L2 -> D1 st. e1 . h21 = q21 . n2
        h21 = concatMap (\(h1,q21,k,r',m1,l') ->
                  let restriction = matchRestrictionToProp (matchRestriction config)
                      hs = findMorphisms restriction (domain n) (codomain k)
                      list = map (\h -> compose h r' == compose n q21) hs in
                       case elemIndex True list of
                           Just ind -> [(h1,q21,k,r',m1,l',hs!!ind)]
                           Nothing  -> [])
                  filtM1

        -- Define m2 = d1 . h21: L2 -> K and abort if m2 not sats NACs r
        m1m2 = map (\(h1,q21,_,r',m1,l',l2d1) -> (q21, h1, compose l2d1 r', m1, compose l2d1 l')) h21
        --filtM2 = filter (\(m1,m2) -> satsNacs r m2) m1m2

        -- Check gluing condition for m2 and r
        filtM2 = filter (\(_,_,_,_,m2) -> satsGluingAndNacs config r m2) m1m2

        in map (\(q21,h1,m2',m1,m2) -> ((m1,m2), (h1,m2'), (q21,idx))) filtM2
