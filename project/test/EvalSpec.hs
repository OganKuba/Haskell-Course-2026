{-# LANGUAGE OverloadedStrings #-}

module EvalSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import BlockChainLang.Eval (evalExpr, initStore, runEval)
import BlockChainLang.Syntax
import BlockChainLang.Value

tests :: TestTree
tests = testGroup "Eval"
  [ testGroup "initStore"
      [ testCase "deploys SimpleCoin with the deployer as owner" $
          initStore simpleCoin (Address "alice") @?= Right expectedStore
      ]

  , testGroup "evalExpr"
      [ testCase "arithmetic" $ do
          run (binop Add 2 3) @?= Right (VInt 5)
          run (binop Sub 10 4) @?= Right (VInt 6)
          run (binop Mul 6 7) @?= Right (VInt 42)

      , testCase "comparisons" $ do
          run (binop Ge 5 3) @?= Right (VBool True)
          run (binop Lt 5 3) @?= Right (VBool False)
          run (binop Eq 4 4) @?= Right (VBool True)

      , testCase "map lookup with a missing key returns the default" $
          runIn balances (Index (Var "balances") (addr "bob")) @?= Right (VInt 0)

      , testCase "map lookup with a present key returns the value" $
          runIn balances (Index (Var "balances") (addr "alice")) @?= Right (VInt 100)

      , testCase "a parameter shadows a state variable" $
          let params = Map.fromList [("x", VInt 9)]
              store  = Map.fromList [("x", VInt 1)]
          in (fst <$> runEval (Address "alice") params store (evalExpr (Var "x")))
               @?= Right (VInt 9)

      , testCase "unbound variable is an error" $
          run (Var "nope") @?= Left (UnboundVar "nope")

      , testCase "type mismatch is an error" $
          case run (BinOp Add (lit 1) (Lit (LBool True))) of
            Left (TypeError _) -> pure ()
            other -> assertFailure ("expected TypeError, got " ++ show other)
      ]
  ]

-- Helpers --------------------------------------------------------------------

run :: Expr -> Either TxError Value
run = runIn Map.empty

runIn :: Store -> Expr -> Either TxError Value
runIn store e = fst <$> runEval (Address "alice") Map.empty store (evalExpr e)

lit :: Integer -> Expr
lit = Lit . LInt

addr :: Text -> Expr
addr = Lit . LAddr

binop :: Op -> Integer -> Integer -> Expr
binop op a b = BinOp op (lit a) (lit b)

balances :: Store
balances = Map.fromList
  [ ("balances", VMap TInt (Map.fromList [(VAddr (Address "alice"), VInt 100)])) ]

-- initStore fixture ----------------------------------------------------------

simpleCoin :: Contract
simpleCoin = Contract
  [ StateVar "balances" (TMap TAddress TInt) Empty
  , StateVar "owner"    TAddress             Sender
  ]
  []

expectedStore :: Store
expectedStore = Map.fromList
  [ ("balances", VMap TInt Map.empty)
  , ("owner",    VAddr (Address "alice"))
  ]
