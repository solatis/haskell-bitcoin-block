{-# LANGUAGE FlexibleInstances  #-}

module Data.Bitcoin.Block.Types where

import Data.Binary ( Binary, get, put )
import Data.Binary.Get ( getByteString
                       , getWord32le )

import Data.Binary.Put ( putByteString
                       , putWord32le )

import Control.Monad ( forM_
                     , replicateM )

import qualified Data.HexString as HS
import qualified Data.ByteString as BS

import Data.Word ( Word32 )
import Data.LargeWord ( Word256 )

import Data.Bitcoin.Types ( VarInt (..)
                          , BlockHash )
import qualified Data.Bitcoin.Transaction as Btc ( Transaction (..)
                                                 , Coinbase )


-- | Data type describing a block in the bitcoin protocol.
data Block = Block {
  -- | Header information for this block.
  blockHeader     :: BlockHeader,

  -- | Coinbase transaction of this block.
  blockCoinbaseTx :: Btc.Coinbase,

  -- | List of transactions pertaining to this block.
  blockTxns       :: [Btc.Transaction]

  } deriving (Eq, Show)

instance Binary Block where

    get = do
        header     <- get
        (VarInt c) <- get
        cb         <- get
        txs        <- replicateM (fromIntegral (c-1)) get
        return $ Block header cb txs

    put (Block h cb txs) = do
        put h
        put $ VarInt $ fromIntegral $ (length txs) + 1
        put cb
        forM_ txs put

-- | Data type recording information on a 'Block'. The hash of a block is
--   defined as the hash of this data structure. The block mining process
--   involves finding a partial hash collision by varying the nonce in the
--   'BlockHeader' and/or additional randomness in the 'Btc.Coinbase' of this
--   'Block'. Variations in the 'Btc.Coinbase' will result in different merkle
--   roots in the 'BlockHeader'.

data BlockHeader = BlockHeader {

  -- | Block version information, based on the version of the
  --   software creating this block.
  blockVersion   :: Word32,

  -- | Hash of the previous block (parent) referenced by this
  --   block.
  prevBlock      :: BlockHash,

  -- | Root of the merkle tree of all transactions pertaining
  --   to this block.
  merkleRoot     :: Word256,

  -- | Unix timestamp recording when this block was created
  blockTimestamp :: Word32,

  -- | The difficulty target being used for this block
  blockBits      :: Word32,

  -- | A random nonce used to generate this block. Additional
  --   randomness is included in the coinbase transaction of
  --   this block.
  bhNonce        :: Word32

  } deriving (Eq, Show)

instance Binary BlockHeader where

    get = do
      v  <- getWord32le
      p  <- getByteString 32 -- A BlockHash is exactly 32 bytes
      m  <- get
      bt <- getWord32le
      bb <- getWord32le
      n  <- getWord32le
      return $ BlockHeader v ((HS.fromBytes . BS.reverse) p) m bt bb n

    put (BlockHeader v p m bt bb n) = do
      putWord32le   v
      putByteString ((BS.reverse . HS.toBytes) p)
      put           m
      putWord32le   bt
      putWord32le   bb
      putWord32le   n
