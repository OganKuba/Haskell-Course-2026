{-# LANGUAGE OverloadedStrings #-}

-- | Transaction dispatch (runtime well-formedness) and a light static check.
module BlockChainLang.Check
  ( dispatch
  , CheckError (..)
  , checkContract
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import BlockChainLang.Eval
import BlockChainLang.Syntax
import BlockChainLang.Value

-- | Find the transaction, check arity and argument types, then run it.
dispatch :: Contract -> Store -> Address -> Text -> [Value] -> Either TxError Store
dispatch contract store sender name args = do
  txdef <- maybe (Left (UnknownTx name)) Right (findTx contract name)
  let ps = txParams txdef
  if length ps /= length args
    then Left (ArityMismatch name (length ps) (length args))
    else do
      params <- buildParams ps args
      runTransaction txdef (EvalCtx sender params) store

findTx :: Contract -> Text -> Maybe TransactionDef
findTx contract name = find ((== name) . T.pack . txName) (contractTransactions contract)

buildParams :: [(String, Type)] -> [Value] -> Either TxError (Map Text Value)
buildParams ps args = Map.fromList <$> traverse checkOne (zip ps args)
  where
    checkOne ((pname, ty), v)
      | matchesType v ty = Right (T.pack pname, v)
      | otherwise =
          Left (TypeError ("argument `" ++ pname ++ "` expected " ++ show ty))

matchesType :: Value -> Type -> Bool
matchesType (VInt _)    TInt          = True
matchesType (VBool _)   TBool         = True
matchesType (VAddr _)   TAddress      = True
matchesType (VMap vt _) (TMap _ vt')  = vt == vt'
matchesType _           _             = False

data CheckError
  = UndeclaredVar   String String
  | BadAssignTarget String Expr
  deriving (Eq, Show)

-- | Every referenced variable must be in scope; assignment targets must be l-values.
checkContract :: Contract -> Either CheckError ()
checkContract (Contract svars txs) = mapM_ checkTx txs
  where
    stateNames = Set.fromList (map svName svars)

    checkTx td =
      let scope = stateNames <> Set.fromList (map fst (txParams td))
      in mapM_ (checkStmt (txName td) scope) (txBody td)

    checkStmt name scope stmt = case stmt of
      Require e      -> checkExpr name scope e
      Assign lhs rhs -> checkTarget name scope lhs >> checkExpr name scope rhs
      If c thn els   -> checkExpr name scope c
                          >> mapM_ (checkStmt name scope) thn
                          >> mapM_ (checkStmt name scope) els

    checkExpr name scope expr = case expr of
      Var x       -> ensure name scope x
      Lit _       -> Right ()
      Sender      -> Right ()
      Empty       -> Right ()
      Index a b   -> checkExpr name scope a >> checkExpr name scope b
      UnOp _ a    -> checkExpr name scope a
      BinOp _ a b -> checkExpr name scope a >> checkExpr name scope b

    checkTarget name scope lhs = case lhs of
      Var x          -> ensure name scope x
      Index (Var m) k -> ensure name scope m >> checkExpr name scope k
      _              -> Left (BadAssignTarget name lhs)

    ensure name scope x
      | x `Set.member` scope = Right ()
      | otherwise            = Left (UndeclaredVar name x)
