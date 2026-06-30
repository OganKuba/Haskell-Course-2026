{-# LANGUAGE OverloadedStrings #-}

-- | The 'Eval' monad: expression evaluation, statement execution, transactions.
module BlockChainLang.Eval
  ( EvalCtx (..)
  , Eval
  , runEval
  , evalExpr
  , execStmt
  , runTransaction
  , initStore
  ) where

import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, execStateT, get, modify, runStateT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import BlockChainLang.Syntax
import BlockChainLang.Value

data EvalCtx = EvalCtx
  { ctxSender :: Address
  , ctxParams :: Map Text Value
  }

-- | A 'Left' (e.g. a failed require) aborts and discards every state change.
type Eval a = ReaderT EvalCtx (StateT Store (Either TxError)) a

runEval :: Address -> Map Text Value -> Store -> Eval a -> Either TxError (a, Store)
runEval sender params store m =
  runStateT (runReaderT m (EvalCtx sender params)) store

evalExpr :: Expr -> Eval Value
evalExpr expr = case expr of
  Var x -> do
    params <- asks ctxParams
    store  <- get
    case Map.lookup (T.pack x) params of
      Just v  -> pure v
      Nothing -> maybe (throwError (UnboundVar x)) pure (Map.lookup (T.pack x) store)
  Lit l  -> pure (litValue l)
  Sender -> VAddr <$> asks ctxSender
  Empty  -> throwError (TypeError "empty map literal needs an expected type")
  Index m k -> do
    mv <- evalExpr m
    kv <- evalExpr k
    case mv of
      VMap vt tbl -> pure (Map.findWithDefault (defaultValue vt) kv tbl)
      _           -> throwError (TypeError "indexing a non-map value")
  UnOp Not e -> do
    v <- evalExpr e
    case v of
      VBool b -> pure (VBool (not b))
      _       -> throwError (TypeError "`not` expects a bool")
  UnOp op _ -> throwError (TypeError ("unary operator " ++ show op))
  BinOp op a b -> do
    va <- evalExpr a
    vb <- evalExpr b
    liftEither (applyBinOp op va vb)

execStmt :: Statement -> Eval ()
execStmt stmt = case stmt of
  Require e -> do
    v <- evalExpr e
    case v of
      VBool True  -> pure ()
      VBool False -> throwError RequireFailed
      _           -> throwError (TypeError "require expects a bool")
  Assign lhs rhs -> do
    v <- evalExpr rhs
    assign lhs v
  If cond thn els -> do
    c <- evalExpr cond
    case c of
      VBool True  -> mapM_ execStmt thn
      VBool False -> mapM_ execStmt els
      _           -> throwError (TypeError "if condition expects a bool")

-- | Store a value at an l-value: a plain variable or one map slot.
assign :: Expr -> Value -> Eval ()
assign (Var x) v = modify (Map.insert (T.pack x) v)
assign (Index (Var m) keyExpr) v = do
  k     <- evalExpr keyExpr
  store <- get
  case Map.lookup (T.pack m) store of
    Just (VMap vt tbl) -> modify (Map.insert (T.pack m) (VMap vt (Map.insert k v tbl)))
    Just _             -> throwError (TypeError ("`" ++ m ++ "` is not a map"))
    Nothing            -> throwError (UnboundVar m)
assign lhs _ = throwError (TypeError ("cannot assign to " ++ show lhs))

runTransaction :: TransactionDef -> EvalCtx -> Store -> Either TxError Store
runTransaction txdef ctx store =
  execStateT (runReaderT (mapM_ execStmt (txBody txdef)) ctx) store

evalInit :: Type -> Expr -> Eval Value
evalInit ty Empty = pure (defaultValue ty)
evalInit _  e     = evalExpr e

-- | Deploy: evaluate each state initialiser with the deployer as sender.
initStore :: Contract -> Address -> Either TxError Store
initStore (Contract svars _) deployer =
  execStateT (runReaderT (mapM_ initVar svars) ctx) Map.empty
  where
    ctx = EvalCtx deployer Map.empty
    initVar sv = do
      v <- evalInit (svType sv) (svInit sv)
      modify (Map.insert (T.pack (svName sv)) v)
