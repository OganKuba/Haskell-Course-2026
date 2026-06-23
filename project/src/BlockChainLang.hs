-- | Umbrella module re-exporting the public API of BlockChainLang.
module BlockChainLang
  ( module BlockChainLang.Syntax
  , module BlockChainLang.Parser
  ) where

import BlockChainLang.Parser
import BlockChainLang.Syntax
