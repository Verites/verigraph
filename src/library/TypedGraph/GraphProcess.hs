module TypedGraph.GraphProcess
( singleProcess
, calculateProcess
, sourcesCoproduct
, allCoproducts
, induceMorphism
, groupMorphisms
)

where

import Abstract.Cocomplete
import Abstract.DPO
import Abstract.DPO.Process
import Abstract.DPO.Derivation
import Abstract.Morphism
import qualified Data.List.NonEmpty as NE
import Graph.GraphMorphism
import TypedGraph.Graph ()
import TypedGraph.GraphRule ()
import TypedGraph.Morphism as TGM

singleProcess :: Derivation (TypedGraphMorphism a b) -> Process (TypedGraphMorphism a b)
singleProcess derivation =
  let d1 = dToG derivation
      d2 = dToH derivation
      (d1',d2') = calculatePushout d1 d2
      core = codomain d1'
      rule = production derivation
      oldL = getLHS rule
      oldR = getRHS rule
      mappingL = mapping oldL
      mappingR = mapping oldR
      m = match derivation
      h = comatch derivation
      newLType = compose (mapping m) (mapping d2')
      newRType = compose (mapping h) (mapping d1')
      newKType = compose mappingL newLType
      newL = buildTypedGraphMorphism newKType newLType mappingL
      newR = buildTypedGraphMorphism newKType newRType mappingR
      newProduction = buildProduction newL newR []
   in Process [newProduction] core

retype :: Derivation (TypedGraphMorphism a b) -> (TypedGraphMorphism a b,TypedGraphMorphism a b,TypedGraphMorphism a b) ->  Production (TypedGraphMorphism a b)
retype derivation (g1,_,g3) = newProduction
  where
    p = production derivation
    oldL = getLHS p
    oldR = getRHS p
    mappingL = mapping oldL
    mappingR = mapping oldR
    m = match derivation
    h = comatch derivation
    newLType = compose (mapping m) (mapping g1)
    newRType = compose (mapping h) (mapping g3)
    newKType = compose mappingL newLType -- change it to use cl2
    newL = buildTypedGraphMorphism newKType newLType mappingL
    newR = buildTypedGraphMorphism newKType newRType mappingR
    newProduction = buildProduction newL newR []

calculateProcess :: [Derivation (TypedGraphMorphism a b)] -> GraphMorphism a b
calculateProcess [] = error "Can not calculate process for empty list of derivations"
calculateProcess ds =
  let fs = sourcesCoproduct ds
      ls = getLefts ds
      rs = getRights ds
      gs = allCoproducts ds
      (g1s, g2s, g3s) = groupMorphisms gs
      h = induceMorphism fs
      h1 = h $ zipWith compose ls g1s
      h2 = h g2s
      h3 = h $ zipWith compose rs g3s
      coEq = calculateNCoequalizer $ NE.fromList [h1,h2,h3]
      core = codomain coEq
   in core

getSources :: [Derivation (TypedGraphMorphism a b)] -> NE.NonEmpty (GraphMorphism a b)
getSources ds = NE.fromList (getDObjects ds)

sourcesCoproduct :: [Derivation (TypedGraphMorphism a b)] -> [TypedGraphMorphism a b]
sourcesCoproduct = calculateNCoproduct . getSources

getAll :: [Derivation (TypedGraphMorphism a b)] -> NE.NonEmpty (GraphMorphism a b)
getAll ds = NE.fromList $ concatMap getAllBottomObjects ds

allCoproducts :: [Derivation (TypedGraphMorphism a b)] -> [TypedGraphMorphism a b]
allCoproducts = calculateNCoproduct . getAll

groupMorphisms :: [TypedGraphMorphism a b] -> ([TypedGraphMorphism a b],[TypedGraphMorphism a b],[TypedGraphMorphism a b])
groupMorphisms [] = ([],[],[])
groupMorphisms fs = (f1,f2,f3)
  where
    groups = reduce fs
    f1 = concatMap (\(a,_,_) -> [a]) groups
    f2 = concatMap (\(_,b,_) -> [b]) groups
    f3 = concatMap (\(_,_,c) -> [c]) groups
    reduce [] = []
    reduce fs = (head fs, head $ tail fs, head $ tail $ tail fs) : reduce (rest fs)
    rest = tail . tail . tail

-- given two TypedGraphMorphism f : A -> B and g : A -> C it induces a Morphism f : B -> C
initialMorphism :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> TypedGraphMorphism a b
initialMorphism f g =
  let typedB = codomain f
      typedC = codomain g
      b = domain typedB
      c = domain typedC
  in buildTypedGraphMorphism typedB typedC (empty b c)

updateRelation :: TypedGraphMorphism a b ->  (TypedGraphMorphism a b, TypedGraphMorphism a b) -> TypedGraphMorphism a b
updateRelation new (f,g) =
  let nodes = nodesFromDomain f
      edges = edgesFromDomain f
      nodeOnB = TGM.applyNodeUnsafe f
      nodeOnC = TGM.applyNodeUnsafe g
      edgeOnB = TGM.applyEdgeUnsafe f
      edgeOnC = TGM.applyEdgeUnsafe g
      newNodeRelation = map (\n -> (nodeOnB n, nodeOnC n)) nodes
      newEdgeRelation = map (\e -> (edgeOnB e, edgeOnC e)) edges
      updateN (s,t) = untypedUpdateNodeRelation s t
      updateE (s,t) = TGM.updateEdgeRelation s t
      n' = foldr updateN new newNodeRelation
   in foldr updateE n' newEdgeRelation

induceMorphism :: [TypedGraphMorphism a b] ->  [TypedGraphMorphism a b] -> TypedGraphMorphism a b
induceMorphism fs gs
  | Prelude.null fs = error "can not induce morphism from empty list of morphisms"
  | length fs /= length gs = error "morphisms list should have the same length"
  | otherwise =
    let h = initialMorphism (head fs) (head gs)
     in foldl updateRelation h (zip fs gs)
