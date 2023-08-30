module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Jyggalag.Toml (parseConfig)
import Data.Functor (void)
import Jyggalag()

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Unit tests"
  [testCase "parse the config file example" $ do
      void $ parseConfig "./jyggalag-example.toml"
  ]
