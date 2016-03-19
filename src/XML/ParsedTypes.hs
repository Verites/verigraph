module XML.ParsedTypes where

type ParsedNode = String -- NodeId
type ParsedTypedNode = (String, String) -- (NodeId, NodeType)
type ParsedEdge = (String, String, String)
type ParsedTypedEdge = (String, String, String, String)
type ParsedGraph = (String, [ParsedNode], [ParsedEdge])
type ParsedTypedGraph = (String, [ParsedTypedNode], [ParsedTypedEdge])
type ParsedRule = (String, String, ParsedTypedGraph,
                   ([ParsedTypedNode], [ParsedTypedEdge]),
                   ([ParsedTypedNode], [ParsedTypedEdge]), [ParsedNAC])
type ParsedNAC = ([ParsedTypedNode], [ParsedTypedEdge])

type TypeGraph = ([ParsedTypedNode], [ParsedTypedEdge])
type Rule = (String, ParsedTypedGraph, ParsedTypedGraph, [Mapping])
type RuleWithNacs = (Rule,[Nac])
type Mapping = (String, String) -- |(image, orig)A
type Nac = (ParsedTypedGraph, [Mapping])

type ConflictMorphism = (ParsedTypedGraph,[Mapping],[Mapping],String,String)
type Overlappings = (String,String,[ConflictMorphism])
-- name rule1, name rule2, morphism, index
type Overlapping = (String,String,ConflictMorphism,Int)
