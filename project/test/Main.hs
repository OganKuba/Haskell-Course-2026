module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified CheckSpec
import qualified EvalSpec
import qualified ParserSpec

main :: IO ()
main = defaultMain $ testGroup "BlockChainLang"
  [ ParserSpec.tests
  , EvalSpec.tests
  , CheckSpec.tests
  ]
