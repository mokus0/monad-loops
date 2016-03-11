{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Loops

testTakeWhileM :: Assertion
testTakeWhileM = do
    actual      <- takeWhileM (return . id) [True, True, False]
    let expected = takeWhile  (         id) [True, True, False]
    actual @?= expected

testTakeWhileMEdgeCase1 :: Assertion
testTakeWhileMEdgeCase1 = do
    actual      <- takeWhileM (return . id) []
    let expected = takeWhile  (         id) []
    actual @?= expected

testTakeWhileMEdgeCase2 :: Assertion
testTakeWhileMEdgeCase2 = do
    actual      <- takeWhileM (return . id) [False, False, False]
    let expected = takeWhile  (         id) [False, False, False]
    actual @?= expected

testTakeWhileMEdgeCase3 :: Assertion
testTakeWhileMEdgeCase3 = do
    let emptyList :: [Int] = []
    actual      <- takeWhileM (const undefined) emptyList
    let expected = takeWhile  (const undefined) emptyList
    actual @?= expected

testTakeIterateM :: Assertion
testTakeIterateM = do
  let n = 3 :: Int
  actual <- takeIterateM n (return . (+ 1)) 0
  expected <- takeIterateM 1 (return . id) n
  actual @?= expected

testTakeIterateMEdgeCase1 :: Assertion
testTakeIterateMEdgeCase1 = do
  let n = 0 :: Int
  actual <- takeIterateM n (return . (+ 1)) 0
  expected <- return n
  actual @?= expected

tests :: TestTree
tests = testGroup "unit tests"
    [ testCase
        "Testing `takeWhileM`"
        testTakeWhileM
    , testCase
        "Testing `takeWhileM (edge case 1)`"
        testTakeWhileMEdgeCase1
    , testCase
        "Testing `takeWhileM (edge case 2)`"
        testTakeWhileMEdgeCase2
    , testCase
        "Testing `takeWhileM (edge case 3)`"
        testTakeWhileMEdgeCase3
    , testCase
        "Testing `takeIterateM`"
        testTakeIterateM
    , testCase
        "Testing `takeIterateM` (edge case 1)"
        testTakeIterateMEdgeCase1
    ]


main :: IO ()
main = defaultMain tests
