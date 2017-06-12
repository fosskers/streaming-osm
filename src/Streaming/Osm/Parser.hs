{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streaming.Osm.Parser where

import           Control.Applicative ((<|>), optional)
import           Control.Monad (void)
import qualified Data.Attoparsec.ByteString as A
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Int
import           Data.List (zipWith4, zipWith7)
import qualified Data.Map as M
import qualified Data.Vector as V
import           Data.Word
import           Streaming.Osm.Types
import           Streaming.Osm.Util
import qualified Data.Attoparsec.Internal.Types as T

---

-- | Parse a `BlobHeader`.
header :: A.Parser ()
header = do
  A.take 4
  A.word8 0x0a
  A.anyWord8
  A.string "OSMHeader" <|> A.string "OSMData"
  optional (A.word8 0x12 *> varint >>= advance)
  void (A.word8 0x18 *> varint @Int)

advance :: Int -> A.Parser ()
advance n = T.Parser $ \t pos more _lose suc -> suc t (pos + T.Pos n) more ()
{-# INLINE advance #-}

blob :: A.Parser Blob
blob = Blob <$> A.eitherP dcmp comp
  where dcmp = A.word8 0x0a *> varint >>= A.take
        comp = (,) <$> (A.word8 0x10 *> varint) <*> (A.word8 0x1a *> varint >>= A.take)

-- | Called a @PrimitiveBlock@ in the OSM literature.
block :: A.Parser Block
block = do
  st <- A.word8 0x0a  *> varint @Int *> stringTable
  ns <- (A.word8 0x12 *> varint @Int *> A.many1' (node st)) <|> pure []
  dn <- (A.word8 0x12 *> varint @Int *> dense st) <|> pure []
  ws <- (A.word8 0x12 *> varint @Int *> A.many1' (way st)) <|> pure []
  rs <- (A.word8 0x12 *> varint @Int *> A.many1' (relation st)) <|> pure []
  gran <- (A.word8 0x88 *> A.word8 0x01 *> varint @Int64) <|> pure 100   -- granularity
  date <- (A.word8 0x90 *> A.word8 0x01 *> varint @Int32) <|> pure 1000  -- date_granularity
  lato <- (A.word8 0x98 *> A.word8 0x01 *> varint @Int64) <|> pure 0     -- lat_offset
  lono <- (A.word8 0xa0 *> A.word8 0x01 *> varint @Int64) <|> pure 0     -- lon_offset
  pure $ Block (map (\f -> f gran date lato lono) $ ns ++ dn)
               (map (\f -> f date) ws)
               (map (\f -> f date) rs)

-- | The String Table will never be empty, since all Elements have
-- non-geographic metadata (username, etc.) which contain Strings. The result
-- must be a `V.Vector`, since we need random access to its contents.
stringTable :: A.Parser (V.Vector B.ByteString)
stringTable = V.fromList <$> A.many1' (A.word8 0x0a *> varint >>= A.take)

-- | Parse a `Node`. Uses `V.unsafeIndex` to quickly retrieve its tag
-- Strings, assuming that the Node's key/value pairs will always index a legal
-- value in the given String Table.
node :: V.Vector B.ByteString -> A.Parser (Int64 -> Int32 -> Int64 -> Int64 -> Node)
node st = do
  A.word8 0x0a *> varint @Int
  i   <- unzig <$> (A.word8 0x08 *> varint)                          -- id
  ks  <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []  -- keys
  vs  <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []  -- vals
  oi  <- optional (A.word8 0x22 *> varint @Int *> info i st)         -- info
  lat <- unzig <$> (A.word8 0x40 *> varint)                          -- lat
  lon <- unzig <$> (A.word8 0x48 *> varint)                          -- lon
  let ts = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
  pure $ (\gran dgran lato lono -> Node (offset lato gran lat) (offset lono gran lon) (($ dgran) <$> oi) ts)

-- | Parse a @DenseNodes@ in a similar way to `node`.
dense :: V.Vector B.ByteString -> A.Parser [Int64 -> Int32 -> Int64 -> Int64 -> Node]
dense st = do
  A.word8 0x12 *> varint @Int
  ids <- undelta . map unzig . packed <$> (A.word8 0x0a *> varint >>= A.take)
  ifs <- (A.word8 0x2a *> varint @Int *> denseInfo ids st) <|> pure (repeat $ const Nothing)
  lts <- undelta . map unzig . packed <$> (A.word8 0x42 *> varint >>= A.take)
  lns <- undelta . map unzig . packed <$> (A.word8 0x4a *> varint >>= A.take)
  kvs <- (packed <$> (A.word8 0x52 *> varint >>= A.take)) <|> pure []
  pure $ zipWith4 f lts lns ifs (denseTags st kvs)
  where f lat lon inf ts = \gran dgran lato lono -> Node (offset lato gran lat) (offset lono gran lon) (inf dgran) ts

-- | Interpret a list of flattened key-value pairs as Tag metadata `Map`s.
denseTags :: V.Vector B.ByteString -> [Int] -> [M.Map B.ByteString B.ByteString]
denseTags _ [] = repeat M.empty
denseTags st kvs = map (M.fromList . map (both (V.unsafeIndex st)) . pairs) $ breakOn0 kvs

-- | Reparse a `B.ByteString` as a list of some Varints.
packed :: (Bits t, Num t) => B.ByteString -> [t]
packed bs = either (const []) id $ A.parseOnly (A.many1' varint) bs
{-# INLINE packed #-}

-- | Parse a `Way`.
way :: V.Vector B.ByteString -> A.Parser (Int32 -> Way)
way st = do
  A.word8 0x1a *> varint @Int
  i <- A.word8 0x08 *> varint                                       -- id
  ks <- packed <$> (A.word8 0x12 *> varint >>= A.take) <|> pure []  -- keys
  vs <- packed <$> (A.word8 0x1a *> varint >>= A.take) <|> pure []  -- vals
  oi <- optional (A.word8 0x22 *> varint @Int *> info i st)         -- info
  ns <- undelta . map unzig . packed <$> (A.word8 0x42 *> varint >>= A.take)
  let ts = M.fromList $ zip (map (V.unsafeIndex st) ks) (map (V.unsafeIndex st) vs)
  pure $ (\dgran -> Way ns (($ dgran) <$> oi) ts)

-- | Parse a `Relation`.
relation :: V.Vector B.ByteString -> A.Parser (Int32 -> Relation)
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
  pure $ (\dgran -> Relation mems (($ dgran) <$> oi) tags)

info :: Int64 -> V.Vector B.ByteString -> A.Parser (Int32 -> Info)
info i st = do
  vn <- ((A.word8 0x08 *> varint) <|> pure (-1))                    -- version
  ts <- optional (A.word8 0x10 *> varint)                           -- timestamp
  cs <- optional (A.word8 0x18 *> varint)                           -- changeset
  ui <- optional (A.word8 0x20 *> varint)                           -- uid
  us <- optional (V.unsafeIndex st <$> (A.word8 0x28 *> varint))    -- user_sid
  vi <- ((>>= booly) <$> optional (A.word8 0x30 *> varint @Word8))  -- visible
  pure $ (\dgran -> Info (fromIntegral i) vn (toffset dgran <$> ts) cs ui us vi)

-- | Parse a @DenseInfo@ message.
denseInfo :: [Int] -> V.Vector B.ByteString -> A.Parser [Int32 -> Maybe Info]
denseInfo nis st = do
  ver <- packed <$> (A.word8 0x0a *> varint >>= A.take)
  tms <- map Just . undelta . map unzig . packed <$> (A.word8 0x12 *> varint >>= A.take)
  chs <- map Just . undelta . map unzig . packed <$> (A.word8 0x1a *> varint >>= A.take)
  uid <- map Just . undelta . map unzig . packed <$> (A.word8 0x22 *> varint >>= A.take)
  uss <- map (st V.!?) . undelta . map unzig . packed <$> (A.word8 0x2a *> varint >>= A.take)
  vis <- (map booly . packed <$> (A.word8 0x32 *> varint >>= A.take)) <|> pure (repeat $ Just True)
  pure . map f $ zipWith7 Info nis ver tms chs uid uss vis
    where f i@(Info _ _ (Just ts) _ _ _ _) dgran = Just $ i { _timestamp = Just $ toffset dgran ts }
          f i _ = Just i

-- | Parse some Varint, which may be made up of multiple bytes.
varint :: (Num a, Bits a) => A.Parser a
varint = foldBytes' <$> A.takeWhile (\b -> testBit b 7) <*> A.anyWord8
{-# INLINE varint #-}

-- | Restore truncated LatLng values to their true `Double` representation.
offset :: Int64 -> Int64 -> Int64 -> Double
offset off gran coord = 0.000000001 * fromIntegral (off + (gran * coord))
{-# INLINE offset #-}

-- | Restore truncated timestamps to the number of millis since the 1970 epoch.
toffset :: Int32 -> Int64 -> Int64
toffset gran time = fromIntegral gran * time

-- TODO: Is this right?
booly :: Word8 -> Maybe Bool
booly 0 = Just False
booly 1 = Just True
booly _ = Nothing

--test :: IO (Either String [B.ByteString])
{-}
test :: IO ()
test = do
  bites <- B.readFile "test/shrine.osm.pbf"
  case A.parseOnly ((,,,) <$> header <*> blob <*> header <*> blob) bites of
    Left err -> putStrLn err
    Right (_, _, _, Blob (Left bs)) -> pPrint $ A.parseOnly block bs
    Right (_, _, _, Blob (Right (_, bs))) -> pPrint . A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs
-}
--    Right (_, _, _, Blob (Right (_, bs))) -> BL.writeFile "SHRINE-BYTES" . decompress $ BL.fromStrict bs

--    where f (Blob { bytes = Left bs }) = A.parseOnly block bs
--          f (Blob { bytes = Right (_, bs) }) = A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs
