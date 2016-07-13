{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Partitions.GraphPart
import           Math.Combinat.Numbers (bellNumber)
import           Test.HUnit

{-data Node = Node {
    ntype    :: Int,
    nname    :: Int,
    nid      :: Int,
    injn     :: Bool,
    inLeftn   :: Bool
    }-}

n1 = Node 1 1 1 True False
n2 = Node 1 2 2 True False
n3 = Node 1 3 3 False True
n4 = Node 1 4 4 False True
l = [n1,n2]

{-data Edge = Edge {
    etype    :: Int,
    label    :: Int,
    eid      :: Int,
    source   :: Node,
    target   :: Node,
    inje     :: Bool,
    inLefte  :: Bool
    } deriving (Eq)-}

e1 = Edge 1 1 1 n1 n2 False False
e2 = Edge 1 1 1 n3 n4 False True
e3 = Edge 1 1 1 n3 n3 False True
e = [e1,e1]

limitBellNumber = 8

tests =
  test ([ "Basic test show" ~: show (genGraphEqClass (l,e)) ~=? "[([[1:1:Right (id:1) {True}],[2:1:Right (id:2) {True}]],[[1:1(1->2)Right (id:1) {False}],[1:1(1->2)Right (id:1) {False}]]),([[1:1:Right (id:1) {True}],[2:1:Right (id:2) {True}]],[[1:1(1->2)Right (id:1) {False},1:1(1->2)Right (id:1) {False}]])]"]
        ++
        -- Tests if a graph with the same repeated node n times has partitions length equals to the n-th bell number
        (map (\n -> ("BellNumber " ++ show n) ~: (length (genGraphEqClass (replicate n n3,[]))) ~=? fromInteger (bellNumber n)) ([0..limitBellNumber] :: [Int]))
        ++
        -- Tests if a graph with the same repeated edge n times has partitions length equals to the n-th bell number
        (map (\n -> ("BellNumber " ++ show n) ~: (length (genGraphEqClass ([n3], replicate n e3))) ~=? fromInteger (bellNumber n)) ([0..limitBellNumber] :: [Int])))
        

main :: IO()
main = do
  
  runTestTT tests  
  
  return ()
