{-# LANGUAGE TypeFamilies #-}
module Graph.Class where

type Node = Int
type LNode a = (Node, a)
type UNode = LNode ()

type Edge = (Int, Node, Node)
type LEdge b = (Int, Node, Node, b)
type UEdge = LEdge ()


class Graph gr where
  type NodeMetadata gr :: *
  type EdgeMetadata gr :: *

  empty :: gr
