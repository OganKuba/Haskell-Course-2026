module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified ParserSpec

main :: IO ()
main = defaultMain $ testGroup "BlockChainLang"
  [ ParserSpec.tests
  ]
