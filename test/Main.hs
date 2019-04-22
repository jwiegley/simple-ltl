module Main where

import LTL
import Test.Tasty
import Test.Tasty.HUnit
import Data.List hiding (and, or)
import Prelude hiding (and, or, until)

assertFormula :: [Int] -> LTL Int -> Assertion
assertFormula xs formula = case run formula xs of
  Right (Just e) -> assertFailure $ "Failed: " ++ show e
  _ -> return ()

assertFormulaComplete :: [Int] -> LTL Int -> Assertion
assertFormulaComplete xs formula = case run formula xs of
  Right (Just e) -> assertFailure $ "Failed: " ++ show e
  Right Nothing -> return ()
  _ -> assertFailure "Incomplete"

main :: IO ()
main = defaultMain $ testGroup "LTL tests"
  [ testCase "even or odd/1" $ assertFormula [1..100] $
      always (test odd `or` (test even `and` next (test odd)))

  , testCase "even or odd/2" $ assertFormula [1..100] $
      always (test odd `until` test even)

  , testCase "even or odd/3" $ assertFormulaComplete [1..2] $
      neg $ test even `and` next (test odd)

  , testCase "subsequent" $ assertFormula [1..100] $
      always (accept (\n -> next (eq (succ n))))
  ]
