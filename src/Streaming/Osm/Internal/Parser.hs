{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Streaming.Osm.Internal.Parser
-- Copyright : (c) Colin Woodbury, 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Streaming.Osm.Internal.Parser where

import           Control.Applicative ((<|>), optional)
import           Control.Monad (void)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as T
import           Data.Bits
import qualified Data.ByteString as B
import           Data.List (zipWith4, zipWith7)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import           Streaming.Osm.Types
import           Streaming.Osm.Internal.Util

---

-- | Parse a `BlobHeader`.
header :: A.Parser ()
header = do
  A.take 4
  A.word8 0x0a
  A.anyWord8
  A.string "OSMHeader" <|> A.string "OSMData"
  optional (A.word8 0x12 *> varint >>= advance)
  void (A.word8 0x18 *> varint)

-- | Borrowed from Attoparsec.
advance :: Int -> A.Parser ()
advance n = T.Parser $ \t pos more _lose suc -> suc t (pos + T.Pos n) more ()
{-# INLINE advance #-}

-- | Called a @Blob@ in the OSM literature.
blob :: A.Parser Blob
blob = Blob <$> A.eitherP dcmp comp
  where dcmp = A.word8 0x0a *> varint >>= A.take
        comp = (,) <$> (A.word8 0x10 *> varint) <*> (A.word8 0x1a *> varint >>= A.take)

-- | Called a @PrimitiveBlock@ in the OSM literature.
block :: A.Parser Block
block = do
  st <- A.word8 0x0a  *> varint *> stringTable
  ns <- (A.word8 0x12 *> varint *> A.many1' (node st)) <|> pure []
  dn <- (A.word8 0x12 *> varint *> dense st) <|> pure []
  ws <- (A.word8 0x12 *> varint *> A.many1' (way st)) <|> pure []
  rs <- (A.word8 0x12 *> varint *> A.many1' (relation st)) <|> pure []
  optional (A.word8 0x88 *> A.word8 0x01 *> varint)  -- granularity
  optional (A.word8 0x90 *> A.word8 0x01 *> varint)  -- date_granularity
  optional (A.word8 0x98 *> A.word8 0x01 *> varint)  -- lat_offset
  optional (A.word8 0xa0 *> A.word8 0x01 *> varint)  -- lon_offset
  pure $ Block (ns ++ dn) ws rs

-- | The String Table will never be empty, since all Elements have
-- non-geographic metadata (username, etc.) which contain Strings. The result
-- must be a `V.Vector`, since we need random access to its contents.
stringTable :: A.Parser (V.Vector B.ByteString)
stringTable = V.fromList <$> A.many1' (A.word8 0x0a *> varint >>= A.take)

-- | Parse a `Node`. Uses `V.unsafeIndex` to quickly retrieve its tag
-- Strings, assuming that the Node's key/value pairs will always index a legal
-- value in the given String Table.
node :: V.Vector B.ByteString -> A.Parser Node
node st = do
  A.word8 0x0a *> varint
  i   <- unzig <$> (A.word8 0x08 *> varint)                          -- id
  ks  <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []  -- keys
  vs  <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []  -- vals
  oi  <- optional (A.word8 0x22 *> varint *> info i st)              -- info
  lat <- unzig <$> (A.word8 0x40 *> varint)                          -- lat
  lon <- unzig <$> (A.word8 0x48 *> varint)                          -- lon
  let ts = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
  pure $ Node (offset lat) (offset lon) oi ts

-- | Parse a @DenseNodes@ in a similar way to `node`.
dense :: V.Vector B.ByteString -> A.Parser [Node]
dense st = do
  A.word8 0x12 *> varint
  ids <- ints <$> (A.word8 0x0a *> varint >>= A.take)
  ifs <- (A.word8 0x2a *> varint *> denseInfo ids st) <|> pure (repeat Nothing)
  lts <- ints <$> (A.word8 0x42 *> varint >>= A.take)
  lns <- ints <$> (A.word8 0x4a *> varint >>= A.take)
  kvs <- (packed <$> (A.word8 0x52 *> varint >>= A.take)) <|> pure []
  pure $ zipWith4 f lts lns ifs (denseTags st kvs)
  where f lat lon inf ts = Node (offset lat) (offset lon) inf ts

-- | Interpret a list of flattened key-value pairs as Tag metadata `Map`s.
denseTags :: V.Vector B.ByteString -> [Int] -> [M.Map B.ByteString B.ByteString]
denseTags _ [] = repeat M.empty
denseTags st kvs = map (M.fromList . map (both (V.unsafeIndex st)) . pairs) $ breakOn0 kvs

-- | Parse a `Way`.
way :: V.Vector B.ByteString -> A.Parser Way
way st = do
  A.word8 0x1a *> varint
  i <- A.word8 0x08 *> varint                                       -- id
  ks <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []  -- keys
  vs <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []  -- vals
  oi <- optional (A.word8 0x22 *> varint *> info i st)              -- info
  ns <- ints <$> (A.word8 0x42 *> varint >>= A.take)
  let ts = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
  pure $ Way ns oi ts

-- | Parse a `Relation`.
relation :: V.Vector B.ByteString -> A.Parser Relation
relation st = do
  A.word8 0x22 *> varint
  i  <- A.word8 0x08 *> varint
  ks <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []                -- keys
  vs <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []                -- vals
  oi <- optional (A.word8 0x22 *> varint *> info i st)                            -- info
  rs <- packed <$> (A.word8 0x42 *> varint >>= A.take) <|> pure []                -- roles_sid
  ms <- map unzig . packed <$> (A.word8 0x4a *> varint >>= A.take) <|> pure []    -- memids
  ts <- map memtype . packed <$> (A.word8 0x52 *> varint >>= A.take) <|> pure []  -- types
  let tags = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
      mems = zipWith3 Member ms ts $ map (V.unsafeIndex st) rs
  pure $ Relation mems oi tags

-- | Parse an `Info`.
info :: Int -> V.Vector B.ByteString -> A.Parser Info
info i st = do
  vn <- (A.word8 0x08 *> varint) <|> pure (-1)                    -- version
  ts <- optional (A.word8 0x10 *> varint)                         -- timestamp
  cs <- optional (A.word8 0x18 *> varint)                         -- changeset
  ui <- optional (A.word8 0x20 *> varint)                         -- uid
  us <- optional (V.unsafeIndex st <$> (A.word8 0x28 *> varint))  -- user_sid
  vi <- (>>= booly) <$> optional (A.word8 0x30 *> varint)         -- visible
  pure $ Info (fromIntegral i) vn (toffset <$> ts) cs ui us vi

-- | Parse a @DenseInfo@ message.
denseInfo :: [Int] -> V.Vector B.ByteString -> A.Parser [Maybe Info]
denseInfo nis st = do
  ver <- packed <$> (A.word8 0x0a *> varint >>= A.take)
  tms <- map Just . ints <$> (A.word8 0x12 *> varint >>= A.take)
  chs <- map Just . ints <$> (A.word8 0x1a *> varint >>= A.take)
  uid <- map Just . ints <$> (A.word8 0x22 *> varint >>= A.take)
  uss <- map (st V.!?) . ints <$> (A.word8 0x2a *> varint >>= A.take)
  vis <- (map booly . packed <$> (A.word8 0x32 *> varint >>= A.take)) <|> pure (repeat $ Just True)
  pure $ zipWith7 f nis ver tms chs uid uss vis
    where f ni vs tm ch ui us vi = Just $ Info ni vs (toffset <$> tm) ch ui us vi

-- | Parse some Varint, which may be made up of multiple bytes.
varint :: A.Parser Int
varint = foldBytes <$> A.takeWhile (`testBit` 7) <*> A.anyWord8
{-# INLINE varint #-}

-- | Reparse a `B.ByteString` as a list of some Varints.
packed :: B.ByteString -> [Int]
packed bs = either (const []) id $ A.parseOnly (A.many1' varint) bs

-- | Decode some packed, Z-encoded, delta-encoded Ints.
ints :: B.ByteString -> [Int]
ints = undelta . map unzig . packed

-- | Restore truncated LatLng values to their true `Double` representation.
offset :: Int -> Double
offset coord = 0.000000001 * fromIntegral (100 * coord)

-- | Restore truncated timestamps to the number of millis since the 1970 epoch.
toffset :: Int -> Int
toffset time = 1000 * time

-- | Try to parse a `Bool` from a bit.
booly :: Int -> Maybe Bool
booly 0 = Just False
booly 1 = Just True
booly _ = Nothing
