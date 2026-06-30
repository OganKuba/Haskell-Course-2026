module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified CheckSpec
import qualified EvalSpec
import qualified HashSpec
import qualified LedgerSpec
import qualified ParserSpec
import qualified Props

main :: IO ()
main = defaultMain $ testGroup "BlockChainLang"
  [ ParserSpec.tests
  , EvalSpec.tests
  , CheckSpec.tests
  , LedgerSpec.tests
  , HashSpec.tests
  , Props.tests
  ]
