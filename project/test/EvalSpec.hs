{-# LANGUAGE OverloadedStrings #-}

module EvalSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import BlockChainLang.Eval (EvalCtx (..), evalExpr, initStore, runEval, runTransaction)
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

  , testGroup "runTransaction"
      [ testCase "a successful transaction commits its assignments" $
          runTransaction (txDef [Assign (Var "x") (lit 5)]) ctx0 store0
            @?= Right (Map.fromList [("x", VInt 5)])

      , testCase "a failed require rolls back earlier assignments" $
          -- assigns x := 5, then aborts; the result is the error, not a
          -- store with x == 5, so nothing committed.
          runTransaction
            (txDef [Assign (Var "x") (lit 5), Require (Lit (LBool False))])
            ctx0 store0
            @?= Left RequireFailed

      , testCase "transfer moves funds between map slots" $
          runTransaction transferTx transferCtx coinStore
            @?= Right (Map.fromList
                  [ ("balances", VMap TInt (Map.fromList
                      [ (VAddr (Address "alice"), VInt 70)
                      , (VAddr (Address "bob"),   VInt 30)
                      ]))
                  ])
      ]
  ]

-- runTransaction fixtures ----------------------------------------------------

ctx0 :: EvalCtx
ctx0 = EvalCtx (Address "alice") Map.empty

store0 :: Store
store0 = Map.fromList [("x", VInt 0)]

txDef :: [Statement] -> TransactionDef
txDef body = TransactionDef "t" [] body

-- transfer(to, amount): balances[sender] -= amount; balances[to] += amount
transferTx :: TransactionDef
transferTx = TransactionDef "transfer" [("to", TAddress), ("amount", TInt)]
  [ Require (BinOp Ge balSender (Var "amount"))
  , Assign balSender (BinOp Sub balSender (Var "amount"))
  , Assign balTo     (BinOp Add balTo     (Var "amount"))
  ]
  where
    balSender = Index (Var "balances") Sender
    balTo     = Index (Var "balances") (Var "to")

transferCtx :: EvalCtx
transferCtx = EvalCtx (Address "alice")
  (Map.fromList [("to", VAddr (Address "bob")), ("amount", VInt 30)])

coinStore :: Store
coinStore = Map.fromList
  [ ("balances", VMap TInt (Map.fromList [(VAddr (Address "alice"), VInt 100)])) ]

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
