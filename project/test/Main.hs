module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BlockChainLang"
  [ testCase "skeleton builds and runs" $ True @?= True
  ]
