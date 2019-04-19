{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Data.ByteString hiding (pack)
import           Data.ByteString.Char8 (pack)
import           LTL
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Regex.PCRE.Heavy

match :: Regex -> LTL String
match regex = accept $ \input -> truth (input =~ regex)

whenMatch :: Regex -> (String -> LTL String) -> LTL String
whenMatch regex k = accept $ \input ->
  case scan regex input of
    (_, x : _xs) : _ -> k x
    _ -> top

compiled :: ByteString -> Regex
compiled regex =
  either (error $ "regexp failed to compile: " ++ show regex) id
    $ compileM regex []

test :: String
test = case go of
  Stop b  -> show b
  Delay _ -> "Delay"
  Ask _   -> "Ask"
 where
  xs = [1,2,3,4] :: [Int]
  go = run (always (implies (eq 2) (bottom "here"))) xs

main :: IO ()
main = defaultMain $ testGroup "LTL tests"
  [ testCase "string match" $ do
      let formula =
              always (whenMatch [re|foo ([0-9]+)|] $ \n ->
                       (match (compiled (pack ("bar " ++ n)))
                        `release`
                        (whenMatch [re|bar ([0-9]+)|] $ \n' ->
                         truth (read n' < (100 :: Int)))))

      case run formula (Prelude.concat (Prelude.replicate 50000
        [ "foo 10"
        , "foo 20"
        , "foo 30"
        , "foo 40"
        , "bar 40"
        , "bar 20"
        , "bar 10"
        , "bar 50"
        , "bar 30" ])) of
          Stop Success -> return ()
          Stop (Failure e) ->
              assertFailure $ "Failed: " ++ show e
          _ ->
            -- If there is a Delay or Query, it means we haven't yet found
            -- evidence yet of an actual error, but may in the future.
            return ()
  ]
