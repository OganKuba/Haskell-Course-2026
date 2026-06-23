-- | Abstract syntax for BlockChainLang.
module BlockChainLang.Syntax
  ( Contract (..)
  , StateVar (..)
  , TransactionDef (..)
  , Type (..)
  , Expr (..)
  , Lit (..)
  , Op (..)
  , Statement (..)
  , Address (..)
  ) where

import Data.Text (Text)

-- | Opaque account identifier.
newtype Address = Address Text
  deriving (Eq, Ord, Show)

data Contract = Contract
  { contractState        :: [StateVar]
  , contractTransactions :: [TransactionDef]
  }
  deriving (Eq, Show)

data StateVar = StateVar
  { svName :: String
  , svType :: Type
  , svInit :: Expr
  }
  deriving (Eq, Show)

data TransactionDef = TransactionDef
  { txName   :: String
  , txParams :: [(String, Type)]
  , txBody   :: [Statement]
  }
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TAddress
  | TMap Type Type
  deriving (Eq, Show)

data Op
  = Add | Sub | Mul
  | Eq | Neq | Lt | Le | Gt | Ge
  | And | Or | Not
  deriving (Eq, Show)

data Lit
  = LInt  Integer
  | LBool Bool
  | LAddr Text
  deriving (Eq, Show)

data Expr
  = Var   String
  | Lit   Lit
  | BinOp Op Expr Expr
  | UnOp  Op Expr
  | Index Expr Expr     -- ^ map indexing: @m[k]@
  | Empty               -- ^ empty map literal
  | Sender              -- ^ submitter of the current transaction
  deriving (Eq, Show)

data Statement
  = Assign  Expr Expr               -- ^ @lhs := rhs@
  | Require Expr
  | If      Expr [Statement] [Statement]
  deriving (Eq, Show)
