import           Abstract.Category.FinitaryCategory
import           Abstract.Rewriting.DPO
import           Analysis.Interlevel.InterLevelCP
import           Category.TypedGraphRule
import           Data.Maybe                                   (fromMaybe)
import           Data.TypedGraph
import           Data.TypedGraph.Morphism
import           Rewriting.DPO.TypedGraphRule.NacManipulation
import           Test.HUnit
import           Util.List
import           Utils
import qualified XML.GGXReader                                as XML

-- | Checks if the number of minimalSafetyNACs was correctly generated.
checkMinimalSafetyNACs log n = [all (== n) list ~?= True]
  where
    list = map snd log

-- | Runs dangling extension to a specific rule and checks if there is
-- the correct number of messages and data nodes, all nodes and all edges
checkDanglingExtension gg1 =
  [ msgsInDang  ~?= 3
  , dataInDang  ~?= 3
  , length nods ~?= 8
  , length edgs ~?= 8
  ]
  where
    ruleC = getRule "ruleC" gg1

    left = getLHS ruleC
    dangGraph = codomain (danglingExtension left)

    [(_,typeOfMsg),(_,typeOfData)] = typedNodes (codomain left)

    msgsInDang = countElement typeOfMsg (map snd nods)
    dataInDang = countElement typeOfData (map snd nods)
    nods = typedNodes dangGraph
    edgs = typedEdges dangGraph

-- | Checks if the inter-level conflicts algorithm finds conflicts in
-- expected situations. It does not check what is the conflict.
checkInterlevelConflict :: MorphismsConfig -> MorphismsConfig -> Grammar (TypedGraphMorphism a b) -> Grammar (RuleMorphism a b) -> [Test]
checkInterlevelConflict mono arbitrary gg1 gg2 =
  [ length (interLevelCP arbitrary a sendMsg) ~?= 1
  , length (interLevelCP arbitrary b sendMsg) ~?= 0
  , length (interLevelCP arbitrary c sendMsg) ~?= 0
  , length (interLevelCP arbitrary d sendMsg) ~?= 0
  , length (interLevelCP arbitrary a getData) ~?= 0
  , length (interLevelCP arbitrary b getData) ~?= 0
  , length (interLevelCP arbitrary c getData) ~?= 0
  , length (interLevelCP arbitrary d getData) ~?= 0
  , length (interLevelCP arbitrary a receiveMsg) ~?= 0
  , length (interLevelCP arbitrary b receiveMsg) ~?= 0
  , length (interLevelCP arbitrary c receiveMsg) ~?= 0
  , length (interLevelCP arbitrary d receiveMsg) ~?= 0
  , length (interLevelCP arbitrary a deleteMsg) ~?= 0
  , length (interLevelCP arbitrary b deleteMsg) ~?= 0
  , length (interLevelCP arbitrary c deleteMsg) ~?= 0
  , length (interLevelCP arbitrary d deleteMsg) ~?= 0
  , length (interLevelCP arbitrary a ruleB) ~?= 0
  , Prelude.null (interLevelCP mono b ruleB) ~?= False
  , length (interLevelCP arbitrary c ruleB) ~?= 0
  , length (interLevelCP arbitrary d ruleB) ~?= 0
  , length (interLevelCP arbitrary a ruleC) ~?= 0
  , length (interLevelCP arbitrary b ruleC) ~?= 0
  , Prelude.null (interLevelCP mono c ruleC) ~?= False
  , length (interLevelCP arbitrary d ruleC) ~?= 0
  , length (interLevelCP arbitrary a ruleD) ~?= 0
  , length (interLevelCP arbitrary b ruleD) ~?= 0
  , length (interLevelCP arbitrary c ruleD) ~?= 0
  , Prelude.null (interLevelCP mono d ruleD) ~?= False
  ]
  where
    sendMsg = ("sendMsg", getRule "sendMsg" gg1)
    getData = ("getData", getRule "getData" gg1)
    receiveMsg = ("receiveMsg", getRule "receiveMsg" gg1)
    deleteMsg = ("deleteMsg", getRule "deleteMsg" gg1)
    ruleB = ("ruleB", getRule "ruleB" gg1)
    ruleC = ("ruleC", getRule "ruleC" gg1)
    ruleD = ("ruleD", getRule "ruleD" gg1)

    a = ("a", getRule "a" gg2)
    b = ("b", getRule "b" gg2)
    c = ("c", getRule "c" gg2)
    d = ("d", getRule "d" gg2)

-- | Checks if the NAC manipulations functions create/delete the
-- expected number of NACs
checkNacManipulation gg =
  [ length createDisable ~?= 0
  , length createPO ~?= 1
  , length createShift ~?= 3
  , length deleteDisable ~?= 3
  , length deleteMono ~?= 0
  , length deleteIPO ~?= 2
  ]
  where
    find :: TypedGraphMorphism a b -> TypedGraphMorphism a b -> [TypedGraphMorphism a b]
    find x y = findAllMorphisms (codomain x) (codomain y)

    -- Creation
    creation_modeledNACs_rule = getRule "creation_modeledNACs" gg
    creation_concreteNACs_rule = getRule "creation_concreteNACs" gg

    match = head (find (getLHS creation_modeledNACs_rule) (getLHS creation_concreteNACs_rule))
    creation_modeledNACs = getNACs creation_modeledNACs_rule

    createDisable = createStep DisableCreate match creation_modeledNACs
    createPO = createStep Pushout match creation_modeledNACs
    createShift = createStep ShiftNACs match creation_modeledNACs

    -- Deletion
    deletion_modeledNACs_rule = getRule "deletion_modeledNACs" gg
    deletion_concreteNACs_rule = getRule "deletion_concreteNACs" gg

    deletion_modeledNACs = getNACs deletion_modeledNACs_rule
    deletion_concreteNACs = getNACs deletion_concreteNACs_rule

    deleteDisable = deleteStep DisableDelete deletion_modeledNACs deletion_concreteNACs
    deleteMono = deleteStep Monomorphisms deletion_modeledNACs deletion_concreteNACs
    deleteIPO = deleteStep InitialPushouts deletion_modeledNACs deletion_concreteNACs

getRule :: String -> Grammar morph -> Production morph
getRule str gg =
  fromMaybe
    (error ("secondOrderTest: " ++ str ++ " is not in secondOrderMatchTest.ggx"))
    (lookup str (productions gg))

main :: IO ()
main = do
  let fileName1 = "tests/grammars/nacs2rule.ggx"
      fileName2 = "tests/grammars/secondOrderMatchTest.ggx"
      fileName3 = "tests/grammars/NacManipulation.ggx"
      dpoConf1 = MorphismsConfig MonoMatches MonomorphicNAC
      dpoConf2 = MorphismsConfig AnyMatches MonomorphicNAC

  (_,_,log1) <- XML.readGrammar fileName1 False dpoConf1
  (_,_,log2) <- XML.readGrammar fileName1 False dpoConf2
  (gg1,gg2,log3) <- XML.readGrammar fileName2 False dpoConf1
  (_,_,log4) <- XML.readGrammar fileName2 False dpoConf2
  (ggNac,_,_) <- XML.readGrammar fileName3 False dpoConf1

  runTests $
    checkMinimalSafetyNACs log1 2 ++
    checkMinimalSafetyNACs log2 9 ++
    checkMinimalSafetyNACs log3 0 ++
    checkMinimalSafetyNACs log4 0 ++
    checkDanglingExtension gg1 ++
    checkNacManipulation ggNac ++
    checkInterlevelConflict dpoConf1 dpoConf2 gg1 gg2
