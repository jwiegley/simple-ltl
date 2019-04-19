{-# LANGUAGE LambdaCase #-}

module Main where

import LTL
import Test.Tasty
import Test.Tasty.HUnit
import Data.List hiding (and, or)
import Prelude hiding (and, or)

main :: IO ()
main = defaultMain $ testGroup "LTL tests"
  [ testCase "string match" $ do
      let formula = always (test odd `or` (test even `and` next (test odd)))
      runMachine (run formula (take 10000000 ([1..] :: [Int])))
          (\case Failure e -> assertFailure $ "Failed: " ++ show e
                 _ -> return ())
          (const (return ()))
          (const (return ()))
  ]
