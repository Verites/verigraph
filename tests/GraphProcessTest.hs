-- | Test Suite for GraphProcess Module

import           Abstract.AdhesiveHLR
import           Abstract.DPO
import           Abstract.DPO.Process
import           Abstract.Morphism
import           Data.Maybe                  (fromJust)
import           Graph.Graph
import           Graph.GraphMorphism
import           Test.HUnit
import           TypedGraph.DPO.GraphProcess ()
import           TypedGraph.Morphism
import           Utils




main :: IO()
main = runTests ("Test with rules" ~: productionsTests)

productionsTests :: Test
productionsTests = test [  "Process From Derivations" ~: processFromDerivations ~=?processFromDerivationsResult
                        ]


-- | Process test with rules FROM: MACHADO, Rodrigo. 2012

typeGraphRules = build [4,3,2,1] [(5,3,4),(4,2,4),(3,2,3),(2,2,1),(1,3,1)]


-- | SendMsg Rule

lSendMsg = build [11,13,14] [(11,13,11)]
lTypedSendMsg = buildGraphMorphism lSendMsg typeGraphRules [(14,4),(13,3),(11,1)] [(11,1)]

kSendMsg = build [21,23,24] []
kTypedSendMsg = buildGraphMorphism kSendMsg typeGraphRules [(24,4),(23,3),(21,1)] []

rSendMsg = build [31,33,34] [(35,33,34)]
rTypedSendMsg = buildGraphMorphism rSendMsg typeGraphRules [(34,4),(33,3),(31,1)] [(35,5)]

kToLMappingSendMsg = buildGraphMorphism kSendMsg lSendMsg [(24,14),(23,13),(21,11)] []
kToRMappingSendMsg = buildGraphMorphism kSendMsg rSendMsg [(24,34),(23,33),(21,31)] []

leftSendMsg = buildTypedGraphMorphism kTypedSendMsg lTypedSendMsg kToLMappingSendMsg
rightSendMsg = buildTypedGraphMorphism kTypedSendMsg rTypedSendMsg kToRMappingSendMsg

sendMsg = buildProduction leftSendMsg rightSendMsg []


-- | GetData Rule

{-getDATA-}
lGetData = build [42,43,44] [(44,42,44),(45,43,44)]
lTypedGetData = buildGraphMorphism lGetData typeGraphRules [(44,4),(43,3),(42,2)] [(44,4),(45,5)]

kGetData = build [52,53,54] [(55,53,54)]
kTypedGetData = buildGraphMorphism kGetData typeGraphRules [(54,4),(53,3),(52,2)] [(55,5)]

rGetData = build [62,63,64] [(65,63,64),(63,62,63)]
rTypedGetData = buildGraphMorphism rGetData typeGraphRules [(64,4),(63,3),(62,2)] [(65,5),(63,3)]

kToLMappingGetData = buildGraphMorphism kGetData lGetData [(52,42),(53,43),(54,44)] [(55,45)]
kToRMappingGetData = buildGraphMorphism kGetData rGetData [(54,64),(53,63),(52,62)] [(55,65)]

leftGetData = buildTypedGraphMorphism kTypedGetData lTypedGetData kToLMappingGetData
rightGetData = buildTypedGraphMorphism kTypedGetData rTypedGetData kToRMappingGetData

getData = buildProduction leftGetData rightGetData []

-- | Test Instantiate

morphismConfig = MorphismsConfig MonoMatches MonomorphicNAC

instanceGraph = build [1,2,3,4] [(1,2,1),(4,4,3)]
typedInstanceGraph = buildGraphMorphism instanceGraph typeGraphRules [(3,4),(2,3),(1,1),(4,2)] [(1,1),(4,4)]

matchSendMsg = head (findMonomorphisms lTypedSendMsg typedInstanceGraph :: [TypedGraphMorphism a b ])
derivationSendMsg =fromJust $  generateDerivation morphismConfig matchSendMsg sendMsg

overlappingGraph = codomain $ comatch derivationSendMsg

matchGetData = head (findMonomorphisms lTypedGetData overlappingGraph :: [TypedGraphMorphism a b])
derivationGetData = fromJust $ generateDerivation morphismConfig matchGetData getData

processFromDerivations = calculateProcess [derivationSendMsg, derivationGetData]


-- | Core Graph Result

coreGraphDomain = build [1,2,3,4] [(1,2,1),(4,4,3),(50,2,3),(196,4,2)]
typedCoreGraph = buildGraphMorphism coreGraphDomain typeGraphRules
                 [(1,1),(2,3),(3,4),(4,2)] [(1,1),(4,4),(50,5),(196,3)]


-- | SendMsg Rule Result

lSendMsgResult = build [11,13,14] [(11,13,11)]
lTypedSendMsgResult = buildGraphMorphism lSendMsgResult coreGraphDomain
                      [(11,1),(13,2),(14,3)] [(11,1)]

kSendMsgResult = build [21,23,24] []
kTypedSendMsgResult = buildGraphMorphism kSendMsgResult coreGraphDomain
                      [(21,1),(23,2),(24,3)] []

rSendMsgResult = build [31,33,34] [(35,33,34)]
rTypedSendMsgResult = buildGraphMorphism rSendMsgResult coreGraphDomain
                      [(31,1),(33,2),(34,3)] [(35,50)]

kToLMappingSendMsgResult = buildGraphMorphism kSendMsgResult lSendMsgResult
                           [(21,11),(23,13),(24,14)] []

kToRMappingSendMsgResult = buildGraphMorphism kSendMsgResult rSendMsgResult
                           [(21,31),(23,33),(24,34)] []

leftSendMsgResult = buildTypedGraphMorphism kTypedSendMsgResult lTypedSendMsgResult kToLMappingSendMsgResult
rightSendMsgResult = buildTypedGraphMorphism kTypedSendMsgResult rTypedSendMsgResult kToRMappingSendMsgResult

sendMsgResult = buildProduction leftSendMsgResult rightSendMsgResult []


-- | GetData Rule Result

lGetDataResult = build [42,43,44] [(44,42,44),(45,43,44)]
lTypedGetDataResult = buildGraphMorphism lGetDataResult coreGraphDomain
                      [(42,4),(43,2),(44,3)] [(44,4),(45,50)]

kGetDataResult = build [52,53,54] [(55,53,54)]
kTypedGetDataResult = buildGraphMorphism kGetDataResult coreGraphDomain
                      [(52,4),(53,2),(54,3)] [(55,50)]

rGetDataResult = build [62,63,64] [(65,63,64),(63,62,63)]
rTypedGetDataResult = buildGraphMorphism rGetDataResult coreGraphDomain
                      [(62,4),(63,2),(64,3)] [(65,50),(63,196)]

kToLMappingGetDataResult = buildGraphMorphism kGetDataResult lGetDataResult
                           [(52,42),(53,43),(54,44)] [(55,45)]

kToRMappingGetDataResult = buildGraphMorphism kGetDataResult rGetDataResult
                           [(52,62),(53,63),(54,64)] [(55,65)]

leftGetDataResult = buildTypedGraphMorphism kTypedGetDataResult lTypedGetDataResult kToLMappingGetDataResult
rightGetDataResult = buildTypedGraphMorphism kTypedGetDataResult rTypedGetDataResult kToRMappingGetDataResult

getDataResult = buildProduction leftGetDataResult rightGetDataResult []


-- | Test Result

processFromDerivationsResult = Process [sendMsgResult, getDataResult] typedCoreGraph
