{-# LANGUAGE OverloadedStrings #-}

module HashSpec (tests) where

import qualified Data.Text as T

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import BlockChainLang.Hash (hashHex)
import BlockChainLang.Ledger
import BlockChainLang.Syntax (Address (..))
import BlockChainLang.Value (Value (..))

tests :: TestTree
tests = testGroup "Hash"
  [ testCase "hashHex is deterministic" $
      hashHex "hello" @?= hashHex "hello"

  , testCase "hashHex is 16 hex characters" $
      T.length (hashHex "anything") @?= 16

  , testCase "different inputs give different digests" $
      assertBool "collision on small inputs" (hashHex "abc" /= hashHex "abd")

  , testCase "mkBlock sets the id to the content hash" $ do
      let b = mkBlock Nothing [tx]
      blockId b @?= blockHash Nothing [tx]

  , testCase "changing the parent changes the id" $
      assertBool "parent not bound into the hash"
        (blockId (mkBlock Nothing [tx]) /= blockId (mkBlock (Just "g") [tx]))

  , testCase "changing the transactions changes the id" $
      assertBool "txs not bound into the hash"
        (blockId (mkBlock Nothing [tx]) /= blockId (mkBlock Nothing []))
  ]

tx :: Tx
tx = Tx (Address "alice") "transfer" [VAddr (Address "bob"), VInt 30]
