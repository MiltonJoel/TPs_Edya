module ListTests where

import Test.HUnit
import Seq
import ListSeq


s0, s1, s2, s3 :: [Int]
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]

testLengthEmptySeq :: Test
testLengthEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence length"
                         0 (lengthS s0)

testLengthNonEmptySeq :: Test
testLengthNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence length"
                         2 (lengthS s2)

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapNonEmptySeq :: Test
testMapNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence map"
                         (fromList [7,4,5]) (mapS (+1) s3)

testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)

testsLists = 
  [
    testMapEmptySeq,
    testMapNonEmptySeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testReduceSumSeq0,
    testReduceSumSeq3,
    testScanSumSeq0,
    testScanSumSeq3
  ]


main :: IO Counts
main = runTestTT $ TestList testsLists

-- 1. Casos de prueba para mapS
testMapS = TestCase (assertEqual "mapS (*2) [1,2,3,4]" 
                     [2,4,6,8] 
                     (mapS (*2) [1,2,3,4 :: Int]))

-- 2. Casos de prueba para filterS
testFilterS = TestCase (assertEqual "filterS even [1,2,3,4,5]" 
                        [2,4] 
                        (filterS even [1,2,3,4,5 :: Int]))

-- 3. Casos de prueba para reduceS
testReduceS = TestCase (assertEqual "reduceS (+) 0 [1,2,3,4]" 
                        10 
                        (reduceS (+) 0 [1,2,3,4 :: Int]))

-- 4. Casos de prueba para scanS
testScanS = TestCase (assertEqual "scanS (+) 0 [1,2,3,4]" 
                      ([0,1,3,6], 10) 
                      (scanS (+) 0 [1,2,3,4 :: Int]))

-- 5. Casos de prueba para joinS
testJoinS = TestCase (assertEqual "joinS [[1,2], [3,4]]" 
                      [1,2,3,4] 
                      (joinS [[1,2], [3,4 :: Int]]))

-- 6. Casos de prueba para tabulateS
testTabulateS = TestCase (assertEqual "tabulateS (^2) 5" 
                          [0,1,4,9,16] 
                          (tabulateS (\x -> x ^ 2) 5))

-- Agrupamos todos los tests
tests = TestList [
    TestLabel "Test de MapS" testMapS,
    TestLabel "Test de FilterS" testFilterS,
    TestLabel "Test de ReduceS" testReduceS,
    TestLabel "Test de ScanS" testScanS,
    TestLabel "Test de JoinS" testJoinS,
    TestLabel "Test de TabulateS" testTabulateS
    ]

main2 :: IO Counts
main2 = runTestTT tests