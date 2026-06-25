{-# LANGUAGE OverloadedStrings #-}

module CheckSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import BlockChainLang.Check
import BlockChainLang.Syntax
import BlockChainLang.Value

tests :: TestTree
tests = testGroup "Check"
  [ testGroup "dispatch"
      [ testCase "a valid call runs the transaction" $
          dispatch coin store (Address "alice") "transfer"
            [VAddr (Address "bob"), VInt 30]
            @?= Right (balancesStore [("alice", 70), ("bob", 30)])

      , testCase "unknown transaction is rejected" $
          dispatch coin store (Address "alice") "mint" [VInt 1]
            @?= Left (UnknownTx "mint")

      , testCase "wrong argument count is rejected" $
          dispatch coin store (Address "alice") "transfer" [VAddr (Address "bob")]
            @?= Left (ArityMismatch "transfer" 2 1)

      , testCase "wrong argument type is rejected" $
          case dispatch coin store (Address "alice") "transfer" [VInt 1, VInt 30] of
            Left (TypeError _) -> pure ()
            other -> assertFailure ("expected TypeError, got " ++ show other)

      , testCase "a failing require (double-spend) is rejected" $
          dispatch coin store (Address "alice") "transfer"
            [VAddr (Address "bob"), VInt 999]
            @?= Left RequireFailed
      ]

  , testGroup "checkContract"
      [ testCase "a well-formed contract passes" $
          checkContract coin @?= Right ()

      , testCase "an undeclared variable is reported" $
          checkContract badCoin @?= Left (UndeclaredVar "transfer" "blances")
      ]
  ]

-- Fixtures -------------------------------------------------------------------

coin :: Contract
coin = Contract
  [ StateVar "balances" (TMap TAddress TInt) Empty ]
  [ transferDef (Index (Var "balances") Sender) ]

-- a copy whose transaction misspells `balances` as `blances`
badCoin :: Contract
badCoin = Contract
  [ StateVar "balances" (TMap TAddress TInt) Empty ]
  [ transferDef (Index (Var "blances") Sender) ]

transferDef :: Expr -> TransactionDef
transferDef balSender =
  TransactionDef "transfer" [("to", TAddress), ("amount", TInt)]
    [ Require (BinOp Ge balSender (Var "amount"))
    , Assign balSender (BinOp Sub balSender (Var "amount"))
    , Assign balTo     (BinOp Add balTo     (Var "amount"))
    ]
  where
    balTo = Index (Var "balances") (Var "to")

store :: Store
store = balancesStore [("alice", 100)]

balancesStore :: [(Text, Integer)] -> Store
balancesStore entries = Map.fromList
  [ ("balances", VMap TInt (Map.fromList
      [ (VAddr (Address a), VInt n) | (a, n) <- entries ])) ]
