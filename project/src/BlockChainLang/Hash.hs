-- | A small non-cryptographic content hash (FNV-1a, 64-bit) for block ids.
module BlockChainLang.Hash
  ( fnv1a
  , hashHex
  ) where

import Data.Bits (xor)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (showHex)

fnv1a :: Text -> Word64
fnv1a = T.foldl' step offsetBasis
  where
    offsetBasis = 0xcbf29ce484222325
    prime       = 0x100000001b3
    step h c = (h `xor` fromIntegral (fromEnum c)) * prime

hashHex :: Text -> Text
hashHex t = T.justifyRight 16 '0' (T.pack (showHex (fnv1a t) ""))
