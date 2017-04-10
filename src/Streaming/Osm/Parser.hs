{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streaming.Osm.Parser where

import           Codec.Compression.Zlib (decompress)
import           Control.Applicative ((<|>), optional)
import qualified Data.Attoparsec.ByteString as A
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Streaming.Osm.Types
import           Streaming.Osm.Util

---

-- | Parse a `BlobHeader`.
header :: A.Parser BlobHeader
header = do
  A.take 4
  A.word8 0x0a
  A.anyWord8
  t <- A.string "OSMHeader" <|> A.string "OSMData"
  i <- optional (A.word8 0x12 *> varint >>= A.take)
  d <- A.word8 0x18 *> varint
  pure $ BlobHeader t i d

blob :: A.Parser Blob
blob = Blob <$> A.eitherP dcmp comp
  where dcmp = A.word8 0x0a *> varint >>= A.take
        comp = (,) <$> (A.word8 0x10 *> varint) <*> (A.word8 0x1a *> varint >>= A.take)

-- This is likely not needed.
headerBlock :: A.Parser [B.ByteString]
headerBlock = do
  optional (A.word8 0x0a *> varint >>= A.take)
  res <- A.many' (A.word8 0x22 *> varint >>= A.take)
  A.many' (A.word8 0x2a *> varint >>= A.take)                   -- optional_features
  optional (A.word8 0x82 *> A.word8 0x01 *> varint >>= A.take)  -- writingprogram
  optional (A.word8 0x8a *> A.word8 0x01 *> varint >>= A.take)  -- source
  optional (A.word8 0x80 *> A.word8 0x02 *> varint @Int64)      -- osmosis_replication_timestamp
  optional (A.word8 0x88 *> A.word8 0x02 *> varint @Int64)      -- osmosis_replication_sequence_number
  optional (A.word8 0x92 *> A.word8 0x02 *> varint >>= A.take)  -- osmosis_replication_bas_url
  pure res

-- | Parse some Varint, which may be made up of multiple bytes.
varint :: (Num a, Bits a) => A.Parser a
varint = foldBytes' <$> A.takeWhile (\b -> testBit b 7) <*> A.anyWord8
{-# INLINABLE varint #-}

test :: IO (Either String [B.ByteString])
test = do
  bytes <- B.readFile "diomede.osm.pbf"
  pure $ A.parseOnly ((,) <$> header <*> blob) bytes >>= f
    where f (_, Blob { bytes = Left bs }) = A.parseOnly headerBlock bs
          f (_, Blob { bytes = Right (_, bs) }) = A.parseOnly headerBlock . BL.toStrict . decompress $ BL.fromStrict bs
