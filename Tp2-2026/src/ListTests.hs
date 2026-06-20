module ListTests where

import Test.HUnit
import Seq
import ListSeq

-- Listas de prueba
s0, s1, s2, s3, s4 :: [Int]
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]
s4 = fromList [1,2,3,4,5]


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
                         (fromList [0,6,9], 13) (scanS (+) 0 s3)

-- Nuevos Tests

testEmptyS :: Test
testEmptyS =
  TestCase $ assertEqual "Error on emptyS" 
                         ([] :: [Int]) emptyS

testSingletonS :: Test
testSingletonS =
  TestCase $ assertEqual "Error on singletonS" 
                         [7] (singletonS 7)

testNthSFirst :: Test
testNthSFirst =
  TestCase $ assertEqual "Error getting first element with nthS" 
                         6 (nthS s3 0)

testNthSLast :: Test
testNthSLast =
  TestCase $ assertEqual "Error getting last element with nthS" 
                         4 (nthS s3 2)

--

testTabulateZero :: Test
testTabulateZero =
  TestCase $ assertEqual "Error tabulating 0 elements" 
                         ([] :: [Int]) (tabulateS (*2) 0)

testTabulateGen :: Test
testTabulateGen =
  TestCase $ assertEqual "Error tabulating 4 elements" 
                         [0, 2, 4, 6] (tabulateS (*2) 4)

testFilterEmpty :: Test
testFilterEmpty =
  TestCase $ assertEqual "Error filtering empty sequence" 
                         [] (filterS even s0)

testFilterNoneMatch :: Test
testFilterNoneMatch =
  TestCase $ assertEqual "Error filtering (no matches)" 
                         [] (filterS (>10) s3)

testFilterSomeMatch :: Test
testFilterSomeMatch =
  TestCase $ assertEqual "Error filtering (some matches)" 
                         [6, 4] (filterS even s3)

--

testAppendEmpty :: Test
testAppendEmpty =
  TestCase $ assertEqual "Error appending empty to empty" 
                         s0 (appendS s0 s0)

testAppendMixed :: Test
testAppendMixed =
  TestCase $ assertEqual "Error appending non-empty to empty" 
                         s3 (appendS s0 s3)

testAppendGen :: Test
testAppendGen =
  TestCase $ assertEqual "Error appending two non-empty sequences" 
                         [5, 1, 6, 3, 4] (appendS s2 s3)

testTakeZero :: Test
testTakeZero =
  TestCase $ assertEqual "Error taking 0 elements" 
                         [] (takeS s3 0)

testTakeGen :: Test
testTakeGen =
  TestCase $ assertEqual "Error taking 2 elements" 
                         [6, 3] (takeS s3 2)

testTakeAll :: Test
testTakeAll =
  TestCase $ assertEqual "Error taking all (or more) elements" 
                         [6, 3, 4] (takeS s3 5)

testDropZero :: Test
testDropZero =
  TestCase $ assertEqual "Error dropping 0 elements" 
                         s3 (dropS s3 0)

testDropGen :: Test
testDropGen =
  TestCase $ assertEqual "Error dropping 2 elements" 
                         [4] (dropS s3 2)

testDropAll :: Test
testDropAll =
  TestCase $ assertEqual "Error dropping all (or more) elements" 
                         [] (dropS s3 5)

--

testShowtEmpty :: Test
testShowtEmpty =
  TestCase $ assertEqual "Error showtS on empty" 
                         (EMPTY :: TreeView Int [Int]) (showtS s0)

testShowtSingleton :: Test
testShowtSingleton =
  TestCase $ assertEqual "Error showtS on singleton" 
                         (ELT 4) (showtS s1)

testShowtEven :: Test
testShowtEven =
  TestCase $ assertEqual "Error showtS on even length" 
                         (NODE [5] [1]) (showtS s2)

testShowtOdd :: Test
testShowtOdd =
  -- splitAt (3 `div` 2) [6,3,4] -> splitAt 1 -> ([6], [3,4])
  TestCase $ assertEqual "Error showtS on odd length" 
                         (NODE [6] [3, 4]) (showtS s3)

testShowlEmpty :: Test
testShowlEmpty =
  TestCase $ assertEqual "Error showlS on empty" 
                         (NIL :: ListView Int [Int]) (showlS s0)

testShowlGen :: Test
testShowlGen =
  TestCase $ assertEqual "Error showlS on sequence" 
                         (CONS 5 [1]) (showlS s2)

--

testJoinEmpty :: Test
testJoinEmpty =
  TestCase $ assertEqual "Error joinS on empty sequence of sequences" 
                         ([] :: [Int]) (joinS [])

testJoinGen :: Test
testJoinGen =
  TestCase $ assertEqual "Error joinS on populated sequence" 
                         [5, 1, 6, 3, 4] (joinS [s2, s0, s3])

testReduceSingleton :: Test
testReduceSingleton =
  TestCase $ assertEqual "Error reducing singleton sequence" 
                         4 (reduceS (+) 0 s1)

testReduceOddLong :: Test
testReduceOddLong =
  TestCase $ assertEqual "Error reducing sequence of length 5" 
                         15 (reduceS (+) 0 s4)

testScanSingleton :: Test
testScanSingleton =
  TestCase $ assertEqual "Error scanS on singleton sequence" 
                         ([0], 4) (scanS (+) 0 s1)

testScanEven :: Test
testScanEven =
  TestCase $ assertEqual "Error scanS on even length sequence" 
                         ([0, 5], 6) (scanS (+) 0 s2)

testScanOddLong :: Test
testScanOddLong =
  TestCase $ assertEqual "Error scanS on sequence of length 5" 
                         ([0, 1, 3, 6, 10], 15) (scanS (+) 0 s4)

--

testsLists = 
  [
    -- Originales
    testMapEmptySeq, testMapNonEmptySeq, testLengthEmptySeq,
    testLengthNonEmptySeq, testReduceSumSeq0, testReduceSumSeq3,
    testScanSumSeq0, testScanSumSeq3,
    
    -- Creación y acceso
    testEmptyS, testSingletonS, testNthSFirst, testNthSLast,
    
    -- Generación y filtrado
    testTabulateZero, testTabulateGen, testFilterEmpty,
    testFilterNoneMatch, testFilterSomeMatch,
    
    -- Modificación
    testAppendEmpty, testAppendMixed, testAppendGen,
    testTakeZero, testTakeGen, testTakeAll,
    testDropZero, testDropGen, testDropAll,
    
    -- Vistas
    testShowtEmpty, testShowtSingleton, testShowtEven, testShowtOdd,
    testShowlEmpty, testShowlGen,
    
    -- Extensión Alto Orden
    testJoinEmpty, testJoinGen, testReduceSingleton, testReduceOddLong,
    testScanSingleton, testScanEven, testScanOddLong
  ]

main :: IO Counts
main = runTestTT $ TestList testsLists