module XML.ParsedTypes where

type ParsedNode = String -- NodeId
type ParsedTypedNode = (String, Maybe String, String) -- (NodeId, name, NodeType)
type ParsedEdge = (String, String, String)
type ParsedTypedEdge = (String, Maybe String, String, String, String) -- (id, name, type, src, tgt)
type ParsedGraph = (String, [ParsedNode], [ParsedEdge])
type ParsedTypedGraph = (String, [ParsedTypedNode], [ParsedTypedEdge])
type ParsedRule = (String, String, ParsedTypedGraph,
                   ([ParsedTypedNode], [ParsedTypedEdge]),
                   ([ParsedTypedNode], [ParsedTypedEdge]), [ParsedNAC])
type ParsedNAC = ([ParsedTypedNode], [ParsedTypedEdge])

type ParsedTypeGraph = ([ParsedTypedNode], [ParsedTypedEdge])
type ParsedAtomicConstraint = (String, ParsedTypedGraph, ParsedTypedGraph, [Mapping])
type Rule = (String, ParsedTypedGraph, ParsedTypedGraph, [Mapping])
type RuleWithNacs = (Rule,[Nac])

-- (side, name, rule)
type SndOrderRuleSide = (String, String, RuleWithNacs)

-- Maybe indicates different prefixes,
-- if is Nothing uses default prefix,
-- otherwise uses Just prefix
type Mapping = (String, Maybe String, String) -- (image, orig prefix, orig)
type Nac = (ParsedTypedGraph, [Mapping])

type ConflictMorphism = (ParsedTypedGraph,[Mapping],[Mapping],String,String)
type Overlappings = (String,String,[ConflictMorphism])
-- name rule1, name rule2, morphism, index
type Overlapping = (String,String,ConflictMorphism,Int)

type Sequence = (String, [SubSequence], [ParsedObjectFlow])
type SubSequence = (Int, [SequenceItem])
type SequenceItem = (Int, String)
type ParsedObjectFlow = (String, String, String, [Mapping])
