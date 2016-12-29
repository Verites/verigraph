module Abstract.DPO.Process
( Process(..)
, GenerateProcess(..))

where

import Abstract.Cocomplete
import Abstract.DPO.Core
import Abstract.DPO.Derivation
import Abstract.Morphism
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (fromJust)
import Grammar.Core

data Process m = Process
  { productions :: [Production m]
  , coreObject :: Obj m
  }

class (DPO m) => GenerateProcess m where

  -- | Given a Derivation @d@ and a tuple @(p,q,r)@ of Morphisms @p : G -> C@, @q : D -> C@ and
  -- @r : H -> C@, it returns a new Production corresponding to the production in @d@ but
  -- typed over C
  typing :: (Derivation m, (m,m,m)) ->  Production m

  -- | Given a Production @p@ and a tuple @(r,s,t)@ of Morphisms @r : G -> C@, @s : D -> C@ and
  -- @t : H -> C@, it returns a new Production corresponding to the production in @p@ but
  -- typed over C
  productionTyping :: (Production m, (m,m,m)) ->  Production m

  -- | Given a list of Derivation corresponding to a sequential derivation, it the returns
  -- the corresponding Process of the Derivations
  calculateProcess :: [Derivation m] -> Process m
  calculateProcess [] = error "Can not calculate process for empty list of derivations"
  calculateProcess ds =
    let fs = sourcesCoproduct ds
        gs = allObjectsCoproduct ds
        (h1,h2,h3) = generateMorphismFamilies ds fs gs
        coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
        hs = reduce $ map (`compose` coEq) gs
     in Process (map typing (zip ds hs)) (codomain coEq)

  generateGrammarProcess :: Grammar m -> Process m
  generateGrammarProcess g =
    let
      rs = extractRules g
      fs = ksCoproduct rs
      gs = allCoproduct rs
      h = induceSpanMorphism fs
      (g1s, g2s, g3s) = groupMorphisms $ split gs
      h1 = h $ zipWith compose (getLefts rs) g1s
      h2 = h g2s
      h3 = h $ zipWith compose (getRights rs) g3s
      coEq = calculateNCoequalizer $ fromList [h1,h2,h3]
      hs = split $ map (`compose` coEq) gs
      in Process (map productionTyping (zip rs hs)) (codomain coEq)

  generateGrammarProcess2 :: (String,[(String, Production m)],[ObjectFlow m]) -> [(String, Production m)]
  generateGrammarProcess2 (_,g,os) =
    let
      --0initial = start g -- original start graph
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
      myfst (a,_,_) = a
      mythd (_,_,a) = a
      leftIOs = map (\o -> compose (snd $ spanMapping o) (myfst $ fromJust (lookup (consumer o) partial))) os
      rightIOs = map (\o -> compose (fst $ spanMapping o) (mythd $ fromJust (lookup (producer o) partial))) os
      objCop = objectFlowCoproduct os
      leftFamily = induceSpanMorphism objCop leftIOs
      rightFamily = induceSpanMorphism objCop rightIOs
      coreGraphMorphism = calculateCoequalizer leftFamily rightFamily
      hs2 = split $ map (`compose` coreGraphMorphism) hm

      newRules = if null os then map productionTyping (zip rs hs1) else map productionTyping (zip rs hs2)
      in (zip ruleNames newRules)-- (codomain coEq)

{-teste :: (GenerateProcess m) => Grammar m -> [ObjectFlow m] -> Grammar m
teste g os =
  let
    initial = start g
    ruleNames = extractRuleNames g
    newRules = productions $ generateGrammarProcess g
    co = zip os (objectFlowCoproduct os)
    in grammar initial [] $ zip ruleNames newRules

abc :: (DPO m) => Grammar m -> [ObjectFlow m] -> ([m],[m])
abc g os = ([],[])
  where
    rs      = rules g
    left o  = codomain $ getLHS $ fromJust $ lookup (output o) rs
    right o = codomain $ getRHS $ fromJust $ lookup (input o)  rs
-}

objectFlowCoproduct :: (DPO m) => [ObjectFlow m] -> [m]
objectFlowCoproduct [] = []
objectFlowCoproduct flows =
  let
    intersectionObjects = fromList $ map (domain . fst . spanMapping) flows
  in calculateNCoproduct intersectionObjects

split :: [m] -> [(m,m,m)]
split [] = []
split (a:b:c:ds) = (a,b,c) : split ds

extractRules :: Grammar m -> [Production m]
extractRules g = map snd (rules g)

extractRuleNames :: Grammar m -> [String]
extractRuleNames g = map fst (rules g)

getKs :: (DPO m) => [Production m] -> [Obj m]
getKs = map (domain . left)

ksCoproduct :: (DPO m) => [Production m] -> [m]
ksCoproduct = calculateNCoproduct . fromList . getKs

allCoproduct :: (DPO m) => [Production m] -> [m]
allCoproduct = calculateNCoproduct . fromList . getAllObjects

getAllObjects :: (DPO m) => [Production m] -> [Obj m]
getAllObjects = foldr (\x -> (++) [(codomain . left) x, (domain . left) x, (codomain . right) x]) []

getLefts :: (DPO m) => [Production m] -> [m]
getLefts = map left

getRights :: (DPO m) => [Production m] -> [m]
getRights = map right

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
