{-# LANGUAGE OverloadedStrings #-}

module EvalSpec (tests) where

import qualified Data.Map.Strict as Map

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import BlockChainLang.Syntax
import BlockChainLang.Value

tests :: TestTree
tests = testGroup "Value"
  [ testCase "initStore deploys SimpleCoin with the deployer as owner" $
      initStore simpleCoin (Address "alice") @?= Right expectedStore
  ]

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
