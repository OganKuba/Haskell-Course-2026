{-# LANGUAGE OverloadedStrings #-}

module LedgerSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import BlockChainLang.Eval (initStore)
import BlockChainLang.Ledger
import BlockChainLang.Syntax
import BlockChainLang.Value

tests :: TestTree
tests = testGroup "Ledger"
  [ testCase "genesis block of transfers updates the queried state" $ do
      let g = mkBlock Nothing
                [ transfer "alice" "bob" 30
                , transfer "alice" "carol" 10
                ]
      currentState <$> submitBlock chain0 g
        @?= Right (balances [("alice", 60), ("bob", 30), ("carol", 10)])

  , testCase "a failed transaction is skipped and the block proceeds" $ do
      let g = mkBlock Nothing
                [ transfer "alice" "bob" 999  -- insufficient funds: skipped
                , transfer "alice" "bob" 30   -- still applied
                ]
      currentState <$> submitBlock chain0 g
        @?= Right (balances [("alice", 70), ("bob", 30)])

  , testCase "a double-spend is rejected and balances stay untouched" $ do
      let g = mkBlock Nothing [transfer "alice" "bob" 999]
      currentState <$> submitBlock chain0 g
        @?= Right (balances [("alice", 100)])

  , testCase "a block naming an unknown parent is rejected" $ do
      let g      = mkBlock Nothing []
          orphan = mkBlock (Just "nope") []
      (submitBlock chain0 g >>= \c -> submitBlock c orphan)
        @?= Left (UnknownParent "nope")

  , testCase "a second parentless block is rejected" $ do
      let g  = mkBlock Nothing [transfer "alice" "bob" 1]
          g2 = mkBlock Nothing []  -- different content => different id
      (submitBlock chain0 g >>= \c -> submitBlock c g2)
        @?= Left (MissingParent (blockId g2))

  , testCase "a child block extends the chain" $ do
      let g  = mkBlock Nothing [transfer "alice" "bob" 30]
          b1 = childBlock g [transfer "bob" "carol" 10]
      (currentState <$> (submitBlock chain0 g >>= \c -> submitBlock c b1))
        @?= Right (balances [("alice", 70), ("bob", 20), ("carol", 10)])

  , testCase "submitting the same block twice is rejected" $ do
      let g = mkBlock Nothing [transfer "alice" "bob" 30]
      (submitBlock chain0 g >>= \c -> submitBlock c g)
        @?= Left (DuplicateBlock (blockId g))

  , testCase "a three-block chain applies transactions in order" $ do
      let g  = mkBlock Nothing      [transfer "alice" "bob" 40]
          b1 = childBlock g         [transfer "bob"   "carol" 25]
          b2 = childBlock b1        [transfer "carol" "alice" 5]
      (currentState <$>
         (submitBlock chain0 g >>= \c1 ->
          submitBlock c1 b1    >>= \c2 ->
          submitBlock c2 b2))
        @?= Right (balances [("alice", 65), ("bob", 15), ("carol", 20)])

  , testCase "newChain deploys the contract before any blocks" $
      (state <$> newChain coin (Address "alice"))
        @?= initStore coin (Address "alice")

  , testCase "a block whose id does not match its content is rejected" $ do
      -- tamper with a well-formed block's transactions, leaving the id stale
      let g    = mkBlock Nothing [transfer "alice" "bob" 30]
          fake = g { txs = [transfer "alice" "bob" 31] }
      submitBlock chain0 fake
        @?= Left (BadBlockId (blockHash Nothing (txs fake)) (blockId fake))
  ]

-- Fixtures -------------------------------------------------------------------

-- A chain whose state is seeded with alice holding 100.
chain0 :: Chain
chain0 = Chain Map.empty (balances [("alice", 100)]) coin

coin :: Contract
coin = Contract
  [ StateVar "balances" (TMap TAddress TInt) Empty ]
  [ TransactionDef "transfer" [("to", TAddress), ("amount", TInt)]
      [ Require (BinOp Ge balSender (Var "amount"))
      , Assign balSender (BinOp Sub balSender (Var "amount"))
      , Assign balTo     (BinOp Add balTo     (Var "amount"))
      ]
  ]
  where
    balSender = Index (Var "balances") Sender
    balTo     = Index (Var "balances") (Var "to")

transfer :: Text -> Text -> Integer -> Tx
transfer from to amount =
  Tx (Address from) "transfer" [VAddr (Address to), VInt amount]

balances :: [(Text, Integer)] -> Store
balances entries = Map.fromList
  [ ("balances", VMap TInt (Map.fromList
      [ (VAddr (Address a), VInt n) | (a, n) <- entries ])) ]
