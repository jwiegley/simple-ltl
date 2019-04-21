module Main where

import LTL
import Criterion.Main
import Data.List hiding (and, or)
import Prelude hiding (and, or)

formula :: LTL Int
formula = always (test odd `or` (test even `and` next (test odd)))

runIt n = run formula (take n ([1..] :: [Int]))

main = defaultMain
  [ bgroup "even/odd"
      [ bench "100000"   $ whnf runIt 100000
      , bench "1000000"  $ whnf runIt 1000000
      , bench "10000000" $ whnf runIt 10000000
      ]
  ]
