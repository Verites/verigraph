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

type Rule = (ParsedTypedGraph, ParsedTypedGraph, [Morphism])
type Morphism = (String, String) -- |(image, orig)
