{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Partitions.GraphPart
import           Test.HUnit

n1 = Node 1 1 1 True False
n2 = Node 1 2 2 True False
n3 = Node 1 3 3 False True
n4 = Node 1 4 4 False True
l = [n1,n2]

e1 = Edge 1 1 1 n1 n2 False False
e2 = Edge 1 1 1 n3 n4 False True
e = [e1,e1]

tests =
  test [ "Basic test show" ~: show (genGraphEqClass (l,e)) ~=? "[([[1:1:Right (id:1) {True}],[2:1:Right (id:2) {True}]],[[1:1(1->2)Right (id:1) {False}],[1:1(1->2)Right (id:1) {False}]]),([[1:1:Right (id:1) {True}],[2:1:Right (id:2) {True}]],[[1:1(1->2)Right (id:1) {False},1:1(1->2)Right (id:1) {False}]])]"
       , "Test 2" ~: "a" ~=? "a"]

main :: IO()
main = do
  
  runTestTT tests

  return ()
