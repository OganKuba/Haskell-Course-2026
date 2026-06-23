{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (tests) where

import Data.List (isInfixOf)
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import BlockChainLang.Parser (parseContract)
import BlockChainLang.Syntax

tests :: TestTree
tests = testGroup "Parser"
  [ testCase "SimpleCoin parses to the expected AST" $
      parseContract simpleCoin @?= Right simpleCoinAst

  , testCase "syntax error reports line and column" $
      case parseContract brokenInput of
        Right _  -> assertBool "expected a parse error" False
        Left err -> assertBool ("missing location in:\n" ++ errorBundlePretty err)
                      ("<input>:4:" `isInfixOf` errorBundlePretty err)
  ]

simpleCoin :: Text
simpleCoin =
  "contract SimpleCoin {\n\
  \  state {\n\
  \    balances: map<address, int> = empty;\n\
  \    owner:    address           = sender;\n\
  \  }\n\
  \\n\
  \  // moves funds between accounts\n\
  \  transaction transfer(to: address, amount: int) {\n\
  \    require balances[sender] >= amount;\n\
  \    balances[sender] := balances[sender] - amount;\n\
  \    balances[to]     := balances[to] + amount;\n\
  \  }\n\
  \}\n"

simpleCoinAst :: Contract
simpleCoinAst = Contract
  [ StateVar "balances" (TMap TAddress TInt) Empty
  , StateVar "owner" TAddress Sender
  ]
  [ TransactionDef "transfer" [("to", TAddress), ("amount", TInt)]
      [ Require (BinOp Ge bal (Var "amount"))
      , Assign bal (BinOp Sub bal (Var "amount"))
      , Assign balTo (BinOp Add balTo (Var "amount"))
      ]
  ]
  where
    bal   = Index (Var "balances") Sender
    balTo = Index (Var "balances") (Var "to")

-- Missing ';' after the initialiser on line 3; the error surfaces at line 4.
brokenInput :: Text
brokenInput =
  "contract Broken {\n\
  \  state {\n\
  \    x: int = 1\n\
  \  }\n\
  \}\n"
