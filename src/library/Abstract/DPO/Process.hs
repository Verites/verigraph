module Abstract.DPO.Process
( Process(..)
, GenerateProcess(..)
, NamedRuleWithMatches
, getRule, getName, getMatch, getComatch)

where

import Abstract.Cocomplete
import Data.Maybe (fromJust)
import Abstract.DPO.Core
import Abstract.DPO.Derivation
import Abstract.Morphism
import Data.List.NonEmpty (NonEmpty, fromList)
import Grammar.Core

data Process m = Process
  { productions :: [Production m]
  , coreObject :: Obj m
  }

class (DPO m) => GenerateProcess m where

  -- | Given a pair of morhisms with common codomain, it returns a new pair with morphism also with a
  -- a new codomain that does not contain the elements that were orphans in both original morphisms
  restrictMorphisms :: (m,m) -> (m,m)

  -- | Given a Derivation @d@ and a tuple @(p,q,r)@ of Morphisms @p : G -> C@, @q : D -> C@ and
  -- @r : H -> C@, it returns a new Production corresponding to the production in @d@ but
  -- typed over C
  typing :: (Derivation m, (m,m,m)) ->  Production m

  -- | Given a Production @p@ and a tuple @(r,s,t)@ of Morphisms @r : G -> C@, @s : D -> C@ and
  -- @t : H -> C@, it returns a new Production corresponding to the production in @p@ but
  -- typed over C
  productionTyping :: (Production m, (m,m,m)) ->  Production m

  -- | Given a list of Derivation containing a sequential derivation, returns its corresponding Process
  calculateProcess :: [Derivation m] -> Process m
  calculateProcess [] = error "Can not calculate process for empty list of derivations"
  calculateProcess ds =
    let fs = sourcesCoproduct ds
        gs = allObjectsCoproduct ds
        (h1,h2,h3) = generateMorphismFamilies ds fs gs
        coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
        hs = reduce $ map (`compose` coEq) gs
     in Process (map typing (zip ds hs)) (codomain coEq)

  calculateRulesColimit :: RuleSequence m -> [NamedRuleWithMatches m]
  calculateRulesColimit (_,g,os) =
    let
      ruleNames = map fst g
      rs = map snd g --- rules
      fs = ksCoproduct rs
      gs = allCoproduct rs
      h = induceSpanMorphism fs
      (g1s, g2s, g3s) = groupMorphisms $ split gs
      h1 = h $ zipWith compose (getLefts rs) g1s
      h2 = h g2s
      h3 = h $ zipWith compose (getRights rs) g3s
      coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
      hm = map (`compose` coEq) gs
      hs1 = split hm -- colimit of the rules themselves


      -- colimit (based on coequalizers) with object flows
      partial = zip ruleNames hs1
      leftIOs = map (\o -> compose (snd $ spanMapping o) (fst' $ fromJust (lookup (consumer o) partial))) os
      rightIOs = map (\o -> compose (fst $ spanMapping o) (thd' $ fromJust (lookup (producer o) partial))) os
      objCop = objectFlowCoproduct os
      leftFamily = induceSpanMorphism objCop leftIOs
      rightFamily = induceSpanMorphism objCop rightIOs
      coreGraphMorphism = calculateCoequalizer leftFamily rightFamily
      hs2 = split $ map (`compose` coreGraphMorphism) hm
    in if null os then zip3 ruleNames rs hs1 else zip3 ruleNames rs hs2

  generateGraphProcess :: RuleSequence m -> [(String, Production m)]
  generateGraphProcess (_,g,os) =
    let
      colimit = calculateRulesColimit ("",g,os)
      ruleNames = map fst g
      newRules = map (productionTyping . forgetRuleName) colimit
      forgetRuleName (_,b,c) = (b,c)
    in zip ruleNames newRules

objectFlowCoproduct :: (DPO m) => [ObjectFlow m] -> [m]
objectFlowCoproduct [] = []
objectFlowCoproduct flows =
  let
    intersectionObjects = fromList $ map (domain . fst . spanMapping) flows
  in calculateNCoproduct intersectionObjects

getLefts :: [Production m] -> [m]
getLefts = map getLHS

getRights :: [Production m] -> [m]
getRights = map getRHS

split :: [m] -> [(m,m,m)]
split [] = []
split (a:b:c:ds) = (a,b,c) : split ds
split _ = error "list of morphisms should have length divisible by 3"

type NamedRuleWithMatches m = (String, Production m, (m,m,m))

getName :: NamedRuleWithMatches m -> String
getName = fst'

getRule :: NamedRuleWithMatches m -> Production m
getRule = snd'

getMatch :: NamedRuleWithMatches m -> m
getMatch = fst' . thd'

getComatch :: NamedRuleWithMatches m -> m
getComatch = thd' . thd'

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

thd' :: (a,b,c) -> c
thd' (_,_,c) = c

generateMorphismFamilies :: (DPO m) => [Derivation m] -> [m] -> [m] -> (m,m,m)
generateMorphismFamilies ds fs gs=
  let ls = getLeftBottomMorphisms ds
      rs = getRightBottomMorphisms ds
      gs' = reduce gs
      (g1s, g2s, g3s) = groupMorphisms gs'
      h = induceSpanMorphism fs
      h1 = h $ zipWith compose ls g1s
      h2 = h g2s
      h3 = h $ zipWith compose rs g3s
  in (h1,h2,h3)

ksCoproduct :: (DPO m) => [Production m] -> [m]
ksCoproduct = calculateNCoproduct . fromList . getKs

allCoproduct :: (DPO m) => [Production m] -> [m]
allCoproduct = calculateNCoproduct . fromList . getAllObjects

getKs :: (DPO m) => [Production m] -> [Obj m]
getKs = map (domain . getLHS)

getAllObjects :: (DPO m) => [Production m] -> [Obj m]
getAllObjects = foldr (\x -> (++) [(codomain . getLHS) x, (domain . getLHS) x, (codomain . getRHS) x]) []

-- | Given a list of Derivation, it returns all the objects in the bottom part of the
-- diagrams that are source of at least one Morphism
getSources :: (DPO m) => [Derivation m] -> NonEmpty (Obj m)
getSources ds = fromList (getDObjects ds)

sourcesCoproduct :: (DPO m) => [Derivation m] -> [m]
sourcesCoproduct = calculateNCoproduct . getSources

-- | Given a list of Derivation, it returns all the objects in the bottom part of the
-- diagrams
getAll :: (DPO m) => [Derivation m] -> NonEmpty (Obj m)
getAll ds = fromList $ getAllBottomObjects ds

allObjectsCoproduct :: (DPO m) => [Derivation m] -> [m]
allObjectsCoproduct = calculateNCoproduct . getAll

-- used to group the morphisms into families
groupMorphisms :: [(m,m,m)] -> ([m],[m],[m])
groupMorphisms [] = ([],[],[])
groupMorphisms fs = (f1,f2,f3)
  where
    f1 = concatMap (\(a,_,_) -> [a]) fs
    f2 = concatMap (\(_,b,_) -> [b]) fs
    f3 = concatMap (\(_,_,c) -> [c]) fs

-- used to separate the list of morphisms in triples for each derivation
reduce :: [m] -> [(m,m,m)]
reduce fs
  | length fs < 3 = []
  | otherwise = (head fs, fs !! 1, fs !! 2) : reduce (rest fs)
    where
      rest = drop 2
