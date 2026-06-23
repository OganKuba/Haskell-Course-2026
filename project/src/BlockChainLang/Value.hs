{-# LANGUAGE OverloadedStrings #-}

-- | Runtime values, the contract store, and contract deployment.
module BlockChainLang.Value
  ( Value (..)
  , Store
  , TxError (..)
  , defaultValue
  , litValue
  , evalExpr
  , evalTyped
  , initStore
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import BlockChainLang.Syntax

-- | A runtime value. 'VMap' carries its /value/ type so that a lookup of a
-- missing key can return the right default (this is what makes
-- @balances[to]@ work for a fresh address).
data Value
  = VInt  Integer
  | VBool Bool
  | VAddr Address
  | VMap  Type (Map Value Value)
  deriving (Eq, Ord, Show)

-- | Contract state: a mapping from state-variable names to values.
type Store = Map Text Value

-- | Errors that abort a transaction (and, with it, any state change).
data TxError
  = RequireFailed
  | UnknownVar  String
  | TypeMismatch String
  deriving (Eq, Show)

-- | The zero value of a type: @0@, @false@, the zero address, the empty map.
defaultValue :: Type -> Value
defaultValue TInt        = VInt 0
defaultValue TBool       = VBool False
defaultValue TAddress    = VAddr (Address "")
defaultValue (TMap _ vt) = VMap vt Map.empty

litValue :: Lit -> Value
litValue (LInt n)  = VInt n
litValue (LBool b) = VBool b
litValue (LAddr t) = VAddr (Address t)

-- | Evaluate a pure expression against a sender and a variable environment.
-- A bare @empty@ has no inferable type here; use 'evalTyped' where a target
-- type is known.
evalExpr :: Address -> Store -> Expr -> Either TxError Value
evalExpr sender env = go
  where
    go expr = case expr of
      Var x   -> maybe (Left (UnknownVar x)) Right (Map.lookup (T.pack x) env)
      Lit l   -> Right (litValue l)
      Sender  -> Right (VAddr sender)
      Empty   -> Left (TypeMismatch "empty map literal needs an expected type")
      Index m k -> do
        mv <- go m
        kv <- go k
        case mv of
          VMap vt tbl -> Right (Map.findWithDefault (defaultValue vt) kv tbl)
          _           -> Left (TypeMismatch "indexing a non-map value")
      UnOp Not e -> do
        v <- go e
        case v of
          VBool b -> Right (VBool (not b))
          _       -> Left (TypeMismatch "`not` expects a bool")
      UnOp op _ -> Left (TypeMismatch ("unary operator " ++ show op))
      BinOp op a b -> do
        va <- go a
        vb <- go b
        evalBin op va vb

evalBin :: Op -> Value -> Value -> Either TxError Value
evalBin op x y = case op of
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
  Not -> Left (TypeMismatch "`not` is a unary operator")
  where
    arith f = case (x, y) of
      (VInt a, VInt b) -> Right (VInt (f a b))
      _                -> Left (TypeMismatch "arithmetic on non-int operands")
    cmp f = case (x, y) of
      (VInt a, VInt b) -> Right (VBool (f a b))
      _                -> Left (TypeMismatch "comparison on non-int operands")
    logic f = case (x, y) of
      (VBool a, VBool b) -> Right (VBool (f a b))
      _                  -> Left (TypeMismatch "logic on non-bool operands")

-- | Like 'evalExpr', but a bare @empty@ takes the given expected type.
evalTyped :: Address -> Store -> Type -> Expr -> Either TxError Value
evalTyped _      _   ty Empty = Right (defaultValue ty)
evalTyped sender env _  e     = evalExpr sender env e

-- | Deploy a contract: evaluate every state initialiser with @deployer@ as
-- @sender@, so e.g. @owner: address = sender@ becomes the deployer. Earlier
-- state variables are in scope for later ones.
initStore :: Contract -> Address -> Either TxError Store
initStore (Contract svars _) deployer = foldM step Map.empty svars
  where
    step st sv = do
      v <- evalTyped deployer st (svType sv) (svInit sv)
      pure (Map.insert (T.pack (svName sv)) v st)
