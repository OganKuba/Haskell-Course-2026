-- | The append-only chain of blocks and the current contract state.
module BlockChainLang.Ledger
  ( BlockId
  , Tx (..)
  , Block (..)
  , Chain (..)
  , LedgerError (..)
  , blockHash
  , mkBlock
  , childBlock
  , newChain
  , submitBlock
  , currentState
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import BlockChainLang.Check (dispatch)
import BlockChainLang.Eval (initStore)
import BlockChainLang.Hash (hashHex)
import BlockChainLang.Syntax
import BlockChainLang.Value

type BlockId = Text

data Tx = Tx
  { txSender :: Address
  , txCall   :: Text
  , txArgs   :: [Value]
  }
  deriving (Eq, Show)

-- | The genesis block is the only one with no parent.
data Block = Block
  { blockId :: BlockId
  , parent  :: Maybe BlockId
  , txs     :: [Tx]
  }
  deriving (Eq, Show)

data Chain = Chain
  { blocks   :: Map BlockId Block
  , state    :: Store
  , contract :: Contract
  }
  deriving (Eq, Show)

data LedgerError
  = DuplicateBlock BlockId
  | UnknownParent  BlockId
  | MissingParent  BlockId
  | BadBlockId     BlockId BlockId
  deriving (Eq, Show)

blockHash :: Maybe BlockId -> [Tx] -> BlockId
blockHash p ts = hashHex (T.pack (show (p, ts)))

mkBlock :: Maybe BlockId -> [Tx] -> Block
mkBlock p ts = Block (blockHash p ts) p ts

childBlock :: Block -> [Tx] -> Block
childBlock prnt = mkBlock (Just (blockId prnt))

newChain :: Contract -> Address -> Either TxError Chain
newChain c deployer = do
  st <- initStore c deployer
  pure (Chain Map.empty st c)

-- | Enforce id/parent invariants, then apply the block's transactions in order.
submitBlock :: Chain -> Block -> Either LedgerError Chain
submitBlock chain blk
  | blockId blk /= expected               = Left (BadBlockId expected (blockId blk))
  | blockId blk `Map.member` blocks chain = Left (DuplicateBlock (blockId blk))
  | otherwise = case parent blk of
      Nothing
        | Map.null (blocks chain) -> Right (commit chain blk)
        | otherwise               -> Left (MissingParent (blockId blk))
      Just pid
        | pid `Map.member` blocks chain -> Right (commit chain blk)
        | otherwise                     -> Left (UnknownParent pid)
  where
    expected = blockHash (parent blk) (txs blk)

commit :: Chain -> Block -> Chain
commit chain blk = chain
  { blocks = Map.insert (blockId blk) blk (blocks chain)
  , state  = foldl' applyTx (state chain) (txs blk)
  }
  where
    -- a failed transaction is skipped; its changes are reverted
    applyTx st tx =
      case dispatch (contract chain) st (txSender tx) (txCall tx) (txArgs tx) of
        Right st' -> st'
        Left _    -> st

currentState :: Chain -> Store
currentState = state
