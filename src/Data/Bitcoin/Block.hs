module Data.Bitcoin.Block ( decode
                          , encode
                          , headerHash
                          , module Data.Bitcoin.Block.Types) where

import           Lens.Micro               ((^.))
import qualified Data.Binary              as B (encode)

import qualified Data.ByteString          as BS (reverse)
import qualified Data.ByteString.Lazy     as BSL (toStrict)

import qualified Crypto.Hash.SHA256       as Sha256
import qualified Data.HexString           as HS

import           Data.Bitcoin.Block.Types

-- | Decodes a hex representation of a transaction into a 'Block' object.
decode :: HS.HexString -- ^ The hexadecimal representation of the transaction
       -> Block        -- ^ The decoded 'Transaction' object
decode = HS.toBinary

-- | Encodes a 'Block' object into a hex representation.
encode :: Block        -- ^ The 'Transaction' we would like to encode to hex
       -> HS.HexString -- ^ The hexadecimal representation of the transaction
encode = HS.fromBinary

-- | Calculates the transaction id of a 'Transaction' as a 'HS.HexString' so it
--   can be used in RPC interfaces.
headerHash :: Block -> HS.HexString
headerHash block =
      -- Bitcoin uses a "double sha256", also known as sha256d as its hash algo
  let sha256d = Sha256.hash . Sha256.hash
      bytes   = BSL.toStrict . B.encode

  in HS.fromBytes . BS.reverse . sha256d . bytes $ (block ^. blockHeader)
