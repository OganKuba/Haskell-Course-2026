{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (tests) where

import Data.List (isInfixOf)
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import BlockChainLang.Parser (parseContract, parseExpr)
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

  , testGroup "expression precedence and associativity"
      [ testCase "* binds tighter than +" $
          parseExpr "1 + 2 * 3" @?= Right (BinOp Add (i 1) (BinOp Mul (i 2) (i 3)))

      , testCase "+ binds tighter than comparison" $
          parseExpr "1 + 2 == 3" @?= Right (BinOp Eq (BinOp Add (i 1) (i 2)) (i 3))

      , testCase "comparison binds tighter than &&" $
          parseExpr "a >= b && c" @?=
            Right (BinOp And (BinOp Ge (Var "a") (Var "b")) (Var "c"))

      , testCase "&& binds tighter than ||" $
          parseExpr "a && b || c" @?=
            Right (BinOp Or (BinOp And (Var "a") (Var "b")) (Var "c"))

      , testCase "not is a prefix that binds tighter than &&" $
          parseExpr "not a && b" @?=
            Right (BinOp And (UnOp Not (Var "a")) (Var "b"))

      , testCase "- is left associative" $
          parseExpr "1 - 2 - 3" @?= Right (BinOp Sub (BinOp Sub (i 1) (i 2)) (i 3))

      , testCase "parentheses override precedence" $
          parseExpr "(1 + 2) * 3" @?= Right (BinOp Mul (BinOp Add (i 1) (i 2)) (i 3))

      , testCase "indexing chains left to right" $
          parseExpr "m[a][b]" @?=
            Right (Index (Index (Var "m") (Var "a")) (Var "b"))
      ]

  , testGroup "literals and atoms"
      [ testCase "boolean literals" $
          parseExpr "true" @?= Right (Lit (LBool True))
      , testCase "address literal" $
          parseExpr "@alice" @?= Right (Lit (LAddr "alice"))
      , testCase "sender and empty" $ do
          parseExpr "sender" @?= Right Sender
          parseExpr "empty"  @?= Right Empty
      ]

  , testGroup "lexing"
      [ testCase "line and block comments are skipped" $
          parseExpr "1 /* block */ + 2 // trailing" @?=
            Right (BinOp Add (i 1) (i 2))

      , testCase "a keyword cannot be used as an identifier" $
          isLeft (parseExpr "contract") @?= True
      ]

  , testCase "a contract with several state vars and transactions parses" $
      isRight (parseContract multi) @?= True
  ]

i :: Integer -> Expr
i = Lit . LInt

isLeft, isRight :: Either a b -> Bool
isLeft  = either (const True)  (const False)
isRight = either (const False) (const True)

multi :: Text
multi =
  "contract Bank {\n\
  \  state {\n\
  \    total:    int                = 0;\n\
  \    balances: map<address, int>  = empty;\n\
  \    open:     bool               = true;\n\
  \  }\n\
  \  transaction deposit(amount: int) {\n\
  \    balances[sender] := balances[sender] + amount;\n\
  \    total := total + amount;\n\
  \  }\n\
  \  transaction close() {\n\
  \    require sender == sender;\n\
  \    open := false;\n\
  \  }\n\
  \}\n"

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
