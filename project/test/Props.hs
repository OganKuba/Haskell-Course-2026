{-# LANGUAGE OverloadedStrings #-}

module Props (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen, checkCoverage, choose, cover, counterexample, elements
  , forAll, listOf, testProperty, (===) )

import BlockChainLang.Ledger
import BlockChainLang.Syntax
import BlockChainLang.Value

tests :: TestTree
tests = testGroup "Properties"
  [ testProperty "SimpleCoin preserves total supply across any transfers" $
      forAll (listOf genTransfer) $ \block ->
        case submitBlock chain0 (mkBlock Nothing block) of
          Left err -> counterexample (show err) False
          Right c  ->
            let st = currentState c
            in -- cover ensures the generator is non-degenerate (funds do move)
               checkCoverage
                 $ cover 30 (st /= seededState) "funds moved"
                 $ totalSupply st === initialTotal
  ]

-- Generators -----------------------------------------------------------------

addresses :: [Text]
addresses = ["alice", "bob", "carol", "dave"]

genTransfer :: Gen Tx
genTransfer = do
  from   <- elements addresses
  to     <- elements addresses
  -- up to 150 against balances of 100, so some requires pass and some fail
  amount <- choose (0, 150)
  pure (Tx (Address from) "transfer" [VAddr (Address to), VInt amount])

-- Fixtures -------------------------------------------------------------------

chain0 :: Chain
chain0 = Chain Map.empty seededState coin

initialTotal :: Integer
initialTotal = totalSupply seededState

seededState :: Store
seededState = balances [(a, 100) | a <- addresses]

totalSupply :: Store -> Integer
totalSupply st = case Map.lookup "balances" st of
  Just (VMap _ m) -> sum [n | VInt n <- Map.elems m]
  _               -> 0

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

balances :: [(Text, Integer)] -> Store
balances entries = Map.fromList
  [ ("balances", VMap TInt (Map.fromList
      [ (VAddr (Address a), VInt n) | (a, n) <- entries ])) ]
