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

import           Abstract.Category.NewClasses
import           Abstract.Rewriting.DPO
import           Abstract.Rewriting.DPO.Derivation
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Maybe                         (fromJust)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set

data Process cat morph = Process
  { productions :: [Production cat morph]
  , coreObject  :: Obj cat
  }

instance (Eq morph, (Eq (Obj cat))) => Eq (Process cat morph) where
 Process p c == Process p' c' = (p,c) == (p',c')
 Process p c /= Process p' c' = (p,c) /= (p',c')

instance (Show morph, (Show (Obj cat))) => Show (Process cat morph) where
  show (Process p c) = "Productions: /n" ++ show p ++
                       "/nCoreObject: /n" ++ show c

data InteractionType = DeleteUse | ProduceForbid | ProduceUse | DeleteForbid deriving (Eq, Show, Ord)

data Interaction = Interaction {
  firstRule       :: String,
  secondRule      :: String,
  interactionType :: InteractionType,
  nacInvolved     :: Maybe Int
} deriving (Eq, Show, Ord)

filterInducedByNacs :: [Interaction] -> Set Interaction
filterInducedByNacs conflictsAndDependencies =
  Set.filter (\i -> interactionType i == ProduceForbid || interactionType i == DeleteForbid) $ Set.fromList conflictsAndDependencies

eliminateSelfConflictsAndDependencies :: [Interaction] -> [Interaction]
eliminateSelfConflictsAndDependencies = filter (\i -> firstRule i /= secondRule i)

class (DPO cat morph) => GenerateProcess cat morph where

  -- | Given a pair of morhisms with common codomain, it returns a new pair with morphism also with a
  -- a new codomain that does not contain the elements that were orphans in both original morphisms
  restrictMorphisms :: (morph,morph) -> (morph,morph)

  -- | Given a morhism, it returns a morphism with a new codomain that is equal to the image of the original
  -- morphism
  restrictMorphism :: morph -> morph

  -- | Given a Derivation @d@ and a tuple @(p,q,r)@ of Morphisms @p : G -> C@, @q : D -> C@ and
  -- @r : H -> C@, it returns a new Production corresponding to the production in @d@ but
  -- typed over C
  typing :: (Derivation cat morph, (morph,morph,morph)) -> Production cat morph

  -- | Given a Production @p@ and a tuple @(r,s,t)@ of Morphisms @r : G -> C@, @s : D -> C@ and
  -- @t : H -> C@, it returns a new Production corresponding to the production in @p@ but
  -- typed over C
  productionTyping :: (Production cat morph, (morph,morph,morph)) ->  Production cat morph

  -- TODO: should calculateProcess be a method?
  -- | Given a list of Derivation containing a sequential derivation, returns its corresponding Process
  calculateProcess :: Cocomplete cat morph => [Derivation cat morph] -> cat (Process cat morph)
  calculateProcess [] = error "Can not calculate process for empty list of derivations"
  calculateProcess ds = do
    fs <- sourcesCoproduct ds
    gs <- allObjectsCoproduct ds
    (h1,h2,h3) <- generateMorphismFamilies ds fs gs
    coEq <- calculateNCoequalizer $ NonEmpty.fromList [h1,h2,h3]
    let hs = reduce $ map (coEq <&> ) gs
    return $ Process (zipWith (curry typing) ds hs) (codomain coEq)

  -- TODO: should calculateRulesColimit be a method?
  calculateRulesColimit :: Cocomplete cat morph => RuleSequence cat morph -> cat [NamedRuleWithMatches cat morph]
  calculateRulesColimit (_,g,os) = do
    let ruleNames = map fst g
        rs = map snd g --- rules
    fs <- ksCoproduct rs
    gs <- allCoproduct rs
    let findH = induceSpanMorphism fs
        (g1s, g2s, g3s) = groupMorphisms $ split gs
    h1 <- findH $ zipWith (<&>) g1s (map leftMorphism rs)
    h2 <- findH g2s
    h3 <- findH $ zipWith (<&>) g3s (map rightMorphism rs)
    coEq <- calculateNCoequalizer $ NonEmpty.fromList [h1,h2,h3]
    let
      hm = map (coEq <&>) gs
      hs1 = split hm -- colimit of the rules themselves


      -- colimit (based on coequalizers) with object flows
      partial = zip ruleNames hs1
      leftIOs  = map (\o -> fst' (fromJust (lookup (consumer o) partial)) <&> snd (spanMapping o)) os
      rightIOs = map (\o -> thd' (fromJust (lookup (producer o) partial)) <&> fst (spanMapping o)) os
    objCop <- objectFlowCoproduct os
    leftFamily <- induceSpanMorphism objCop leftIOs
    rightFamily <- induceSpanMorphism objCop rightIOs
    coreGraphMorphism <- calculateCoequalizer leftFamily rightFamily
    let hs2 = split $ map (coreGraphMorphism <&>) hm
    return $ if null os then zip3 ruleNames rs hs1 else zip3 ruleNames rs hs2

  -- TODO: should generateGraphProcess be a method?
  generateGraphProcess :: Cocomplete cat morph => RuleSequence cat morph -> cat [(String, Production cat morph)]
  generateGraphProcess (_,g,os) = do
    colimit <- calculateRulesColimit ("",g,os)
    let ruleNames = map fst g
        newRules = map (productionTyping . forgetRuleName) colimit
        forgetRuleName (_,b,c) = (b,c)
    return (zip ruleNames newRules)

objectFlowCoproduct :: (DPO cat morph, Cocomplete cat morph) => [ObjectFlow cat morph] -> cat [morph]
objectFlowCoproduct [] = return []
objectFlowCoproduct flows =
  let
    intersectionObjects = map (domain . fst . spanMapping) flows
  in calculateNCoproduct intersectionObjects

split :: [morph] -> [(morph,morph,morph)]
split []         = []
split (a:b:c:ds) = (a,b,c) : split ds
split _          = error "list of morphisms should have length divisible by 3"

type NamedRuleWithMatches cat morph = (String, Production cat morph, (morph,morph,morph))

getName :: NamedRuleWithMatches cat morph -> String
getName = fst'

getRule :: NamedRuleWithMatches cat morph -> Production cat morph
getRule = snd'

getMatch :: NamedRuleWithMatches cat morph -> morph
getMatch = fst' . thd'

getComatch :: NamedRuleWithMatches cat morph -> morph
getComatch = thd' . thd'

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

thd' :: (a,b,c) -> c
thd' (_,_,c) = c

generateMorphismFamilies :: (DPO cat morph) => [Derivation cat morph] -> [morph] -> [morph] -> cat (morph,morph,morph)
generateMorphismFamilies ds fs gs = do
  let ls = getLeftBottomMorphisms ds
      rs = getRightBottomMorphisms ds
      gs' = reduce gs
      (g1s, g2s, g3s) = groupMorphisms gs'
      findH = induceSpanMorphism fs
  h1 <- findH $ zipWith (<&>) g1s ls
  h2 <- findH g2s
  h3 <- findH $ zipWith (<&>) g3s rs
  return (h1, h2, h3)

ksCoproduct :: (DPO cat morph, Cocomplete cat morph) => [Production cat morph] -> cat [morph]
ksCoproduct = calculateNCoproduct . map interfaceObject

allCoproduct :: (DPO cat morph, Cocomplete cat morph) => [Production cat morph] -> cat [morph]
allCoproduct = calculateNCoproduct . getAllObjects

getAllObjects :: (DPO cat morph) => [Production cat morph] -> [Obj cat]
getAllObjects = concatMap (\x -> [leftObject x, interfaceObject x, rightObject x])

sourcesCoproduct :: (DPO cat morph, Cocomplete cat morph) => [Derivation cat morph] -> cat [morph]
sourcesCoproduct = calculateNCoproduct . getDObjects

allObjectsCoproduct :: (DPO cat morph, Cocomplete cat morph) => [Derivation cat morph] -> cat [morph]
allObjectsCoproduct = calculateNCoproduct . getAllBottomObjects

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
