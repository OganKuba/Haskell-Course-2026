{-# LANGUAGE OverloadedStrings #-}

-- | The transaction evaluator: the 'Eval' monad and expression evaluation.
--
-- The stack is @ReaderT EvalCtx (StateT Store (Either TxError))@. Read-only
-- context (the sender and the transaction parameters) lives in the 'ReaderT';
-- the mutable contract state lives in the 'StateT'; a 'Left' aborts the whole
-- transaction, so callers that discard the failed run keep the prior state
-- with no manual rollback.
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

-- | Read-only evaluation context.
data EvalCtx = EvalCtx
  { ctxSender :: Address
  , ctxParams :: Map Text Value
  }

type Eval a = ReaderT EvalCtx (StateT Store (Either TxError)) a

-- | Run an evaluation, returning its result and the final store (or the error
-- that aborted it).
runEval :: Address -> Map Text Value -> Store -> Eval a -> Either TxError (a, Store)
runEval sender params store m =
  runStateT (runReaderT m (EvalCtx sender params)) store

-- | Evaluate a pure expression. 'Var' is looked up in the parameters first,
-- then in the store; a missing map key yields the value type's default.
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

-- | Execute a single statement, mutating the store.
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

-- | Store a value at an l-value: either a plain variable or one map slot.
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

-- | Run a transaction body against a store. The whole body runs in
-- @StateT Store (Either TxError)@, so a 'throwError' (e.g. a failed
-- 'Require') yields 'Left' and discards every change the body made — the
-- caller simply keeps the store it passed in. No manual rollback.
runTransaction :: TransactionDef -> EvalCtx -> Store -> Either TxError Store
runTransaction txdef ctx store =
  execStateT (runReaderT (mapM_ execStmt (txBody txdef)) ctx) store

-- | A bare @empty@ takes its type from the surrounding declaration.
evalInit :: Type -> Expr -> Eval Value
evalInit ty Empty = pure (defaultValue ty)
evalInit _  e     = evalExpr e

-- | Deploy a contract: evaluate every state initialiser with @deployer@ as
-- @sender@. Earlier state variables are in scope for later ones.
initStore :: Contract -> Address -> Either TxError Store
initStore (Contract svars _) deployer =
  execStateT (runReaderT (mapM_ initVar svars) ctx) Map.empty
  where
    ctx = EvalCtx deployer Map.empty
    initVar sv = do
      v <- evalInit (svType sv) (svInit sv)
      modify (Map.insert (T.pack (svName sv)) v)
