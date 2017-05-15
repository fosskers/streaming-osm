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
import           Data.List (zipWith5, zipWith7)
import qualified Data.Map as M
import qualified Data.Vector as V
import           Data.Word
import           Streaming.Osm.Types
import           Streaming.Osm.Util
import           Text.Pretty.Simple (pPrint)

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
  A.many'  (A.word8 0x2a *> varint >>= A.take)                  -- optional_features
  optional (A.word8 0x82 *> A.word8 0x01 *> varint >>= A.take)  -- writingprogram
  optional (A.word8 0x8a *> A.word8 0x01 *> varint >>= A.take)  -- source
  optional (A.word8 0x80 *> A.word8 0x02 *> varint @Int64)      -- osmosis_replication_timestamp
  optional (A.word8 0x88 *> A.word8 0x02 *> varint @Int64)      -- osmosis_replication_sequence_number
  optional (A.word8 0x92 *> A.word8 0x02 *> varint >>= A.take)  -- osmosis_replication_bas_url
  pure res

-- TODO: many or many' ?

-- | Called a @PrimitiveBlock@ in the OSM literature.
block :: A.Parser Block
block = do
  st <- A.word8 0x0a *> varint @Int *> stringTable
  ns <- (A.word8 0x12 *> varint @Int *> A.many1 (node st)) <|> pure []
  dn <- (A.word8 0x12 *> varint @Int *> dense st) <|> pure []
  ws <- (A.word8 0x12 *> varint @Int *> A.many1 (way st)) <|> pure []
  rs <- (A.word8 0x12 *> varint @Int *> A.many1 (relation st)) <|> pure []
  gran <- (A.word8 0x88 *> A.word8 0x01 *> varint @Int64) <|> pure 100   -- granularity
  date <- (A.word8 0x90 *> A.word8 0x01 *> varint @Int32) <|> pure 1000  -- date_granularity
  lato <- (A.word8 0x98 *> A.word8 0x01 *> varint @Int64) <|> pure 0     -- lat_offset
  lono <- (A.word8 0xa0 *> A.word8 0x01 *> varint @Int64) <|> pure 0     -- lon_offset
  pure $ Block (map (\f -> f gran lato lono) $ ns ++ dn) ws rs

-- | The String Table will never be empty, since all Elements have
-- non-geographic metadata (username, etc.) which contain Strings. The result
-- must be a `V.Vector`, since we need random access to its contents.
stringTable :: A.Parser (V.Vector B.ByteString)
stringTable = V.fromList <$> A.many1 (A.word8 0x0a *> varint >>= A.take)

-- | Parse a `Node`. Uses `V.unsafeIndex` to quickly retrieve its tag
-- Strings, assuming that the Node's key/value pairs will always index a legal
-- value in the given String Table.
node :: V.Vector B.ByteString -> A.Parser (Int64 -> Int64 -> Int64 -> Node)
node st = do
  A.word8 0x0a *> varint @Int
  i   <- unzig <$> (A.word8 0x08 *> varint)                          -- id
  ks  <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []  -- keys
  vs  <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []  -- vals
  oi  <- optional (A.word8 0x22 *> varint @Int *> info i st)         -- info
  lat <- unzig <$> (A.word8 0x40 *> varint)                          -- lat
  lon <- unzig <$> (A.word8 0x48 *> varint)                          -- lon
  let ts = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
  pure $ (\gran lato lono -> Node (offset lato gran lat) (offset lono gran lon) oi ts)

-- | Parse a @DenseNodes@ in a similar way to `node`.
dense :: V.Vector B.ByteString -> A.Parser [Int64 -> Int64 -> Int64 -> Node]
dense st = do
  A.word8 0x12 *> varint @Int
  ids <- undelta . map unzig . packed <$> (A.word8 0x0a *> varint >>= A.take)
  ifs <- (A.word8 0x2a *> varint @Int *> denseInfo ids st) <|> pure (repeat Nothing)
  lts <- undelta . map unzig . packed <$> (A.word8 0x42 *> varint >>= A.take)
  lns <- undelta . map unzig . packed <$> (A.word8 0x4a *> varint >>= A.take)
  kvs <- (packed <$> (A.word8 0x52 *> varint >>= A.take)) <|> pure []
  pure $ zipWith5 f ids lts lns ifs (denseTags st kvs)
  where f i lat lon inf ts = \gran lato lono -> Node (offset lato gran lat) (offset lono gran lon) inf ts

-- | Interpret a list of flattened key-value pairs as Tag metadata `Map`s.
denseTags :: V.Vector B.ByteString -> [Int] -> [M.Map B.ByteString B.ByteString]
denseTags st = map (M.fromList . map (both (V.unsafeIndex st)) . pairs) . breakOn0

-- | Reparse a `B.ByteString` as a list of some Varints.
packed :: (Bits t, Num t) => B.ByteString -> [t]
packed bs = either (const []) id $ A.parseOnly (A.many1 varint) bs
{-# INLINABLE packed #-}

-- | Parse a `Way`.
way :: V.Vector B.ByteString -> A.Parser Way
way st = do
  A.word8 0x1a *> varint @Int
  i <- A.word8 0x08 *> varint                                       -- id
  ks <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []  -- keys
  vs <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []  -- vals
  oi <- optional (A.word8 0x22 *> varint @Int *> info i st)         -- info
  ns <- undelta . map unzig . packed <$> (A.word8 0x42 *> varint >>= A.take)
  let ts = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
  pure $ Way ns oi ts

-- | Parse a `Relation`.
relation :: V.Vector B.ByteString -> A.Parser Relation
relation st = do
  A.word8 0x22 *> varint @Int
  i <- A.word8 0x08 *> varint
  ks <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []                -- keys
  vs <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []                -- vals
  oi <- optional (A.word8 0x22 *> varint @Int *> info i st)                       -- info
  rs <- packed <$> (A.word8 0x42 *> varint >>= A.take) <|> pure []                -- roles_sid
  ms <- map unzig . packed <$> (A.word8 0x4a *> varint >>= A.take) <|> pure []    -- memids
  ts <- map memtype . packed <$> (A.word8 0x52 *> varint >>= A.take) <|> pure []  -- types
  let tags = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
      mems = zipWith3 Member ms ts $ map (V.unsafeIndex st) rs
  pure $ Relation mems oi tags

-- TODO: Timestamp offsets.
info :: Int64 -> V.Vector B.ByteString -> A.Parser Info
info i st = Info
  <$> pure (fromIntegral i)
  <*> ((A.word8 0x08 *> varint) <|> pure (-1))
  <*> optional (A.word8 0x10 *> varint)
  <*> optional (A.word8 0x18 *> varint)
  <*> optional (A.word8 0x20 *> varint)
  <*> optional (V.unsafeIndex st <$> (A.word8 0x28 *> varint))
  <*> ((>>= booly) <$> optional (A.word8 0x30 *> varint @Word8))

-- | Parse a @DenseInfo@ message.
denseInfo :: [Int] -> V.Vector B.ByteString -> A.Parser [Maybe Info]
denseInfo nis st = do
  ver <- packed <$> (A.word8 0x0a *> varint >>= A.take)
  tms <- map Just . undelta . map unzig . packed <$> (A.word8 0x12 *> varint >>= A.take)
  chs <- map Just . undelta . map unzig . packed <$> (A.word8 0x1a *> varint >>= A.take)
  uid <- map Just . undelta . map unzig . packed <$> (A.word8 0x22 *> varint >>= A.take)
  uss <- map (st V.!?) . undelta . map unzig . packed <$> (A.word8 0x2a *> varint >>= A.take)
  vis <- (map booly . packed <$> (A.word8 0x32 *> varint >>= A.take)) <|> pure (repeat $ Just True)
  pure . map Just $ zipWith7 Info nis ver tms chs uid uss vis

-- | Parse some Varint, which may be made up of multiple bytes.
varint :: (Num a, Bits a) => A.Parser a
varint = foldBytes' <$> A.takeWhile (\b -> testBit b 7) <*> A.anyWord8
{-# INLINABLE varint #-}

-- | Restore truncated LatLng values to their true `Double` representation.
offset :: Int64 -> Int64 -> Int64 -> Double
offset off gran coord = 0.000000001 * fromIntegral (off + (gran * coord))

-- TODO: Is this right?
booly :: Word8 -> Maybe Bool
booly 0 = Just False
booly 1 = Just True
booly _ = Nothing

--test :: IO (Either String [B.ByteString])
test :: IO ()
test = do
  bytes <- B.readFile "diomede.osm.pbf"
  case A.parseOnly ((,,,) <$> header <*> blob <*> header <*> blob) bytes of
    Left err -> putStrLn err
    Right (_, _, _, Blob (Left bs)) -> pPrint $ A.parseOnly block bs
    Right (_, _, _, Blob (Right (_, bs))) -> pPrint . fmap relations . A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs
--    Right (_, _, _, Blob (Right (_, bs))) -> BL.writeFile "SHRINE-BYTES" . decompress $ BL.fromStrict bs

--    where f (Blob { bytes = Left bs }) = A.parseOnly block bs
--          f (Blob { bytes = Right (_, bs) }) = A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs
