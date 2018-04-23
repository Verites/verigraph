{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Abstract.Rewriting.DPO.Process
  ( Process(..)
  , GenerateProcess(..)
  , NamedRuleWithMatches
  , Interaction (..)
  , InteractionType (..)
  , getRule, getName, getMatch, getComatch
  , filterInducedByNacs
  , eliminateSelfConflictsAndDependencies
  ) where

import           Abstract.Category
import           Abstract.Category.Limit
import           Abstract.Category.FindMorphism
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.Derivation
import           Data.List.NonEmpty                 (NonEmpty, fromList)
import           Data.Maybe                         (fromJust)
import qualified Data.Set                           as S

data Process morph = Process
  { productions :: [Production morph]
  , coreObject  :: Obj morph
  }

instance (Eq morph, (Eq (Obj morph))) => Eq (Process morph) where
 Process p c == Process p' c' = (p,c) == (p',c')
 Process p c /= Process p' c' = (p,c) /= (p',c')

instance (Show morph, (Show (Obj morph))) => Show (Process morph) where
  show (Process p c) = "Productions: \n" ++ show p ++
                       "\nCoreObject: \n" ++ show c

data InteractionType = DeleteUse | ProduceForbid | ProduceUse | DeleteForbid deriving (Eq, Show, Ord)

data Interaction = Interaction {
  firstRule       :: String,
  secondRule      :: String,
  interactionType :: InteractionType,
  nacInvolved     :: Maybe Int
} deriving (Eq, Show, Ord)

filterInducedByNacs :: [Interaction] -> S.Set Interaction
filterInducedByNacs conflictsAndDependencies =
  S.filter (\i -> interactionType i == ProduceForbid || interactionType i == DeleteForbid) $ S.fromList conflictsAndDependencies

eliminateSelfConflictsAndDependencies :: [Interaction] -> [Interaction]
eliminateSelfConflictsAndDependencies = filter (\i -> firstRule i /= secondRule i)

class (DPO morph, Cocomplete morph) => GenerateProcess morph where

  -- | Given a pair of morhisms with common codomain, it returns a new pair with morphism also with a
  -- a new codomain that does not contain the elements that were orphans in both original morphisms
  restrictMorphisms :: (morph,morph) -> (morph,morph)

  -- | Given a morhism, it returns a morphism with a new codomain that is equal to the image of the original
  -- morphism
  restrictMorphism :: morph -> morph

  -- | Given a Derivation @d@ and a tuple @(p,q,r)@ of Morphisms @p : G -> C@, @q : D -> C@ and
  -- @r : H -> C@, it returns a new Production corresponding to the production in @d@ but
  -- typed over C
  typing :: (Derivation morph, (morph,morph,morph)) ->  Production morph

  -- | Given a Production @p@ and a tuple @(r,s,t)@ of Morphisms @r : G -> C@, @s : D -> C@ and
  -- @t : H -> C@, it returns a new Production corresponding to the production in @p@ but
  -- typed over C
  productionTyping :: (Production morph, (morph,morph,morph)) ->  Production morph

  -- | Given a list of Derivation containing a sequential derivation, returns its corresponding Process
  calculateProcess :: [Derivation morph] -> Process morph
  calculateProcess [] = error "Can not calculate process for empty list of derivations"
  calculateProcess ds =
    let fs = sourcesCoproduct ds
        gs = allObjectsCoproduct ds
        (h1,h2,h3) = generateMorphismFamilies ds fs gs
        coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
        hs = reduce $ map (coEq <&> ) gs
     in Process (zipWith (curry typing) ds hs) (codomain coEq)

  calculateRulesColimit :: RuleSequence morph -> [NamedRuleWithMatches morph]
  calculateRulesColimit (_,g,os) =
    let
      ruleNames = map fst g
      rs = map snd g --- rules
      fs = ksCoproduct rs
      gs = allCoproduct rs
      h = induceSpanMorphism fs
      (g1s, g2s, g3s) = groupMorphisms $ split gs
      h1 = h $ zipWith invertedCompose (getLefts rs) g1s
      h2 = h g2s
      h3 = h $ zipWith invertedCompose (getRights rs) g3s
      coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
      hm = map (coEq <&> ) gs
      hs1 = split hm -- colimit of the rules themselves


      -- colimit (based on coequalizers) with object flows
      partial = zip ruleNames hs1
      leftIOs  = map (\o -> fst' (fromJust (lookup (consumer o) partial)) <&> snd (spanMapping o)) os
      rightIOs = map (\o -> thd' (fromJust (lookup (producer o) partial)) <&> fst (spanMapping o)) os
      objCop = objectFlowCoproduct os
      leftFamily = induceSpanMorphism objCop leftIOs
      rightFamily = induceSpanMorphism objCop rightIOs
      coreGraphMorphism = calculateCoequalizer leftFamily rightFamily
      hs2 = split $ map (coreGraphMorphism <&>) hm
    in if null os then zip3 ruleNames rs hs1 else zip3 ruleNames rs hs2

  generateGraphProcess :: RuleSequence morph -> [(String, Production morph)]
  generateGraphProcess (_,g,os) =
    let
      colimit = calculateRulesColimit ("",g,os)
      ruleNames = map fst g
      newRules = map (productionTyping . forgetRuleName) colimit
      forgetRuleName (_,b,c) = (b,c)
    in zip ruleNames newRules

objectFlowCoproduct :: (DPO morph, Cocomplete morph) => [ObjectFlow morph] -> [morph]
objectFlowCoproduct [] = []
objectFlowCoproduct flows =
  let
    intersectionObjects = fromList $ map (domain . fst . spanMapping) flows
  in calculateNCoproduct intersectionObjects

getLefts :: [Production morph] -> [morph]
getLefts = map leftMorphism

getRights :: [Production morph] -> [morph]
getRights = map rightMorphism

split :: [morph] -> [(morph,morph,morph)]
split []         = []
split (a:b:c:ds) = (a,b,c) : split ds
split _          = error "list of morphisms should have length divisible by 3"

type NamedRuleWithMatches morph = (String, Production morph, (morph,morph,morph))

getName :: NamedRuleWithMatches morph -> String
getName = fst'

getRule :: NamedRuleWithMatches morph -> Production morph
getRule = snd'

getMatch :: NamedRuleWithMatches morph -> morph
getMatch = fst' . thd'

getComatch :: NamedRuleWithMatches morph -> morph
getComatch = thd' . thd'

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

thd' :: (a,b,c) -> c
thd' (_,_,c) = c

invertedCompose :: Category morph => morph -> morph -> morph
invertedCompose a b = b <&> a

generateMorphismFamilies :: (DPO morph) => [Derivation morph] -> [morph] -> [morph] -> (morph,morph,morph)
generateMorphismFamilies ds fs gs=
  let ls = getLeftBottomMorphisms ds
      rs = getRightBottomMorphisms ds
      gs' = reduce gs
      (g1s, g2s, g3s) = groupMorphisms gs'
      h = induceSpanMorphism fs
      h1 = h $ zipWith invertedCompose ls g1s
      h2 = h g2s
      h3 = h $ zipWith invertedCompose rs g3s
  in (h1,h2,h3)

ksCoproduct :: (DPO morph, Cocomplete morph) => [Production morph] -> [morph]
ksCoproduct = calculateNCoproduct . fromList . getKs

allCoproduct :: (DPO morph, Cocomplete morph) => [Production morph] -> [morph]
allCoproduct = calculateNCoproduct . fromList . getAllObjects

getKs :: (DPO morph) => [Production morph] -> [Obj morph]
getKs = map interfaceObject

getAllObjects :: (DPO morph) => [Production morph] -> [Obj morph]
getAllObjects = foldr (\x -> (++) [leftObject x, interfaceObject x, rightObject x]) []

-- | Given a list of Derivation, it returns all the objects in the bottom part of the
-- diagrams that are source of at least one Morphism
getSources :: (DPO morph) => [Derivation morph] -> NonEmpty (Obj morph)
getSources ds = fromList (getDObjects ds)

sourcesCoproduct :: (DPO morph, Cocomplete morph) => [Derivation morph] -> [morph]
sourcesCoproduct = calculateNCoproduct . getSources

-- | Given a list of Derivation, it returns all the objects in the bottom part of the
-- diagrams
getAll :: (DPO morph) => [Derivation morph] -> NonEmpty (Obj morph)
getAll ds = fromList $ getAllBottomObjects ds

allObjectsCoproduct :: (DPO morph, Cocomplete morph) => [Derivation morph] -> [morph]
allObjectsCoproduct = calculateNCoproduct . getAll

-- used to group the morphisms into families
groupMorphisms :: [(morph,morph,morph)] -> ([morph],[morph],[morph])
groupMorphisms [] = ([],[],[])
groupMorphisms fs = (f1,f2,f3)
  where
    f1 = concatMap (\(a,_,_) -> [a]) fs
    f2 = concatMap (\(_,b,_) -> [b]) fs
    f3 = concatMap (\(_,_,c) -> [c]) fs

-- used to separate the list of morphisms in triples for each derivation
reduce :: [morph] -> [(morph,morph,morph)]
reduce fs
  | length fs < 3 = []
  | otherwise = (head fs, fs !! 1, fs !! 2) : reduce (rest fs)
    where
      rest = drop 2
