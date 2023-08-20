module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Poppen.Toml (parseConfig)
import Data.Functor (void)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Unit tests"
  [testCase "parse config file" $ do
      void $ parseConfig "./poppen.toml"
  ]
