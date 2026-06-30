{-# LANGUAGE OverloadedStrings #-}

-- | Runtime values, the contract store, and value-level primitives.
module BlockChainLang.Value
  ( Value (..)
  , Store
  , TxError (..)
  , defaultValue
  , litValue
  , applyBinOp
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import BlockChainLang.Syntax

-- | 'VMap' carries its value type so a missing-key lookup can return a default.
data Value
  = VInt  Integer
  | VBool Bool
  | VAddr Address
  | VMap  Type (Map Value Value)
  deriving (Eq, Ord, Show)

type Store = Map Text Value

data TxError
  = TypeError      String
  | UnboundVar     String
  | RequireFailed
  | UnknownTx      Text
  | ArityMismatch  Text Int Int
  deriving (Eq, Show)

defaultValue :: Type -> Value
defaultValue TInt        = VInt 0
defaultValue TBool       = VBool False
defaultValue TAddress    = VAddr (Address "")
defaultValue (TMap _ vt) = VMap vt Map.empty

litValue :: Lit -> Value
litValue (LInt n)  = VInt n
litValue (LBool b) = VBool b
litValue (LAddr t) = VAddr (Address t)

applyBinOp :: Op -> Value -> Value -> Either TxError Value
applyBinOp op x y = case op of
  Add -> arith (+)
  Sub -> arith (-)
  Mul -> arith (*)
  Eq  -> Right (VBool (x == y))
  Neq -> Right (VBool (x /= y))
  Lt  -> cmp (<)
  Le  -> cmp (<=)
  Gt  -> cmp (>)
  Ge  -> cmp (>=)
  And -> logic (&&)
  Or  -> logic (||)
  Not -> Left (TypeError "`not` is a unary operator")
  where
    arith f = case (x, y) of
      (VInt a, VInt b) -> Right (VInt (f a b))
      _                -> Left (TypeError "arithmetic on non-int operands")
    cmp f = case (x, y) of
      (VInt a, VInt b) -> Right (VBool (f a b))
      _                -> Left (TypeError "comparison on non-int operands")
    logic f = case (x, y) of
      (VBool a, VBool b) -> Right (VBool (f a b))
      _                  -> Left (TypeError "logic on non-bool operands")
