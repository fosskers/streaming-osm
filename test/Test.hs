{-# LANGUAGE LambdaCase #-}

module Main where

import           Codec.Compression.Zlib (decompress)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import           Streaming.Osm
import           Streaming.Osm.Internal.Parser
import           Streaming.Osm.Internal.Util
import           Streaming.Osm.Types
import qualified Streaming.Prelude as S
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Util"
    [ testGroup "foldBytes"
      [ testCase "3" $ foldBytes (BS.pack [0x03]) 0 @?= 3
      , testCase "124" $ foldBytes (BS.pack [0x7c]) 0 @?= 124
      , testCase "150" $ foldBytes (BS.pack [0x96, 0x01]) 0 @?= 150
      , testCase "270" $ foldBytes (BS.pack [0x8e, 0x02]) 0 @?= 270
      , testCase "86942" $ foldBytes (BS.pack [0x9e, 0xa7, 0x05]) 0 @?= 86942
      ]
    -- , testGroup "key"
    --   [ testCase "08" $ key @Word8 0x08 @?= (1, 0)
    --   , testCase "51" $ key @Word8 0x51 @?= (10, 1)
    --   , testCase "12" $ key @Word8 0x12 @?= (2, 2)
    --   , testCase "1a" $ key @Word8 0x1a @?= (3, 2)
    --   , testCase "22" $ key @Word8 0x22 @?= (4, 2)
    --   , testCase "55" $ key @Word8 0x55 @?= (10, 5)
    --   ]
    -- , testGroup "key2"
    --   [ testCase "82 01" $ key2 0x82 0x01 @?= (16, 2)
    --   , testCase "92 02" $ key2 0x92 0x02 @?= (34, 2)
    --   ]
    -- , testGroup "unkey"
    --   [ testCase "34 string" $ unkey 34 2 @?= Left (0x92, 0x02)
    --   , testCase "16 string" $ unkey 16 2 @?= Left (0x82, 0x01)
    --   , testCase "01 varint" $ unkey 01 0 @?= Right 0x08
    --   , testCase "02 string" $ unkey 02 2 @?= Right 0x12
    --   , testCase "03 string" $ unkey 03 2 @?= Right 0x1a
    --   , testCase "04 string" $ unkey 04 2 @?= Right 0x22
    --   , testCase "10 64bit"  $ unkey 10 1 @?= Right 0x51
    --   , testCase "10 32bit"  $ unkey 10 5 @?= Right 0x55
    --   ]
    , testGroup "breakOn0"
      [ testCase "Simple" $ breakOn0 [0,0,0,0,7,2,9,5,0] @?= [[], [], [], [], [7,2,9,5]]
      ]
--    , testGroup "groupBytes"
--      [ testCase "03 8E 02 9E A7 05" groupBytesT
--      ]
    ]
  , testGroup "Parser functions"
    [ testCase "StringTable" stringTableT
    , testCase "DenseNodes" denseNodesT
    , testCase "DenseInfo" denseInfoT
    , testCase "varint" $ varintT ids
    , testCase "varint" $ varintT lts
    , testCase "varint" $ varintT lns
    , testCase "way" wayT
    ]
  , testGroup "Parsing Whole Files"
    [ testCase "shrine" shrineT
    , testCase "diomede" diomedeT
    , testCase "tashirojima" tashirojimaT
    , testCase "ajishima" ajishimaT
    , testCase "nozakijima" nozakijimaT
    ]
  , testGroup "Streaming"
    [ testCase "shrine" $ fileS "test/shrine.osm.pbf" (5, 1, 0)
    , testCase "tashirojima" $ fileS "test/tashirojima.osm.pbf" (4040, 384, 2)
    , testCase "nozakijima" $ fileS "test/nozakijima.osm.pbf" (3371, 151, 8)
    , testCase "ajishima" $ fileS "test/ajishima.osm.pbf" (5118, 325, 1)
    , testCase "uku" $ fileS "test/uku.osm.pbf" (17390, 1228, 6)
    , testCase "North Van" $ fileS "test/north-van.osm.pbf" (48596, 7757, 52)
--    , testCase "Vancouver" $ fileS "test/vancouver.osm.pbf" (804749, 156053, 1689)
    ]
  , testGroup "Misc."
    [ testCase "Relation Ref Values: Diomede" diomedeRefValues
    , testCase "Relation Ref Values: Ajishima" ajishimaRefValues
    ]
  ]

assertRight :: Either t1 t -> Assertion
assertRight (Left _) = assertFailure "Crap"
assertRight _        = pure ()

-- | Bytes which represent a `StringTable`.
st :: BS.ByteString
st = BS.pack [ 0x0a, 0x00, 0x0a, 0x07, 0x54, 0x6f, 0x6d, 0x5f, 0x47, 0x33, 0x58, 0x0a, 0x06, 0x73
             , 0x6f, 0x75, 0x72, 0x63, 0x65, 0x0a, 0x0c, 0x79, 0x68, 0x3a, 0x53, 0x54, 0x52, 0x55
             , 0x43, 0x54, 0x55, 0x52, 0x45, 0x0a, 0x09, 0x63, 0x6f, 0x61, 0x73, 0x74, 0x6c, 0x69
             , 0x6e, 0x65, 0x0a, 0x07, 0x6e, 0x61, 0x74, 0x75, 0x72, 0x61, 0x6c, 0x0a, 0x0f, 0xe9
             , 0x80, 0x9a, 0xe5, 0xb8, 0xb8, 0xe6, 0xb0, 0xb4, 0xe6, 0xb6, 0xaf, 0xe7, 0xb7, 0x9a
             , 0x0a, 0x07, 0x79, 0x68, 0x3a, 0x54, 0x59, 0x50, 0x45, 0x0a, 0x12, 0x59, 0x61, 0x68
             , 0x6f, 0x6f, 0x4a, 0x61, 0x70, 0x61, 0x6e, 0x2f, 0x41, 0x4c, 0x50, 0x53, 0x4d, 0x41
             , 0x50, 0x0a, 0x09, 0xe6, 0xb5, 0xb7, 0xe5, 0xb2, 0xb8, 0xe7, 0xb7, 0x9a ]

-- | Bytes which represent `DenseNodes`.
dn :: BS.ByteString
dn = BS.pack [
  -- Header stuff
  0x12, 0x5f
  -- Packed list of IDs
  , 0x0a, 0x0a
  , 0x9c, 0xb3, 0xaf, 0xde, 0x0a, 0x04, 0x10, 0x16, 0x02, 0x10
  -- Embedded `DenseInfo`
  , 0x2a, 0x31, 0x0a, 0x06, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x12, 0x0a, 0x9e, 0xcf
  , 0xde, 0xe7, 0x09, 0x00, 0x02, 0x00, 0x00, 0x02, 0x1a, 0x09, 0xe2, 0x99, 0xf8, 0x08
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x22, 0x08, 0xe4, 0xf7, 0x11, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x2a, 0x06, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00
  -- Packed list of LATs
  , 0x42, 0x0d
  , 0x98, 0x88, 0x8a, 0xbc, 0x02, 0x78, 0xac, 0x02, 0xf8, 0x05, 0x28, 0x90, 0x03
  -- Packed list of LNGs
  , 0x4a, 0x0f
  , 0xf4, 0xd4, 0xc6, 0xcf, 0x09, 0xa7, 0x0a, 0xe4, 0x0f, 0xf3, 0x0d, 0xe8, 0x0c, 0x8b, 0x06 ]

-- | Bytes which represent `DenseInfo`.
dinf :: BS.ByteString
dinf = BS.pack [
  -- Versions
  0x0a, 0x06
  , 0x01, 0x01, 0x01, 0x01, 0x01, 0x01
  -- Timestamps
  , 0x12, 0x0a
  , 0x9e, 0xcf, 0xde, 0xe7, 0x09, 0x00, 0x02, 0x00, 0x00, 0x02
  -- Changesets
  , 0x1a, 0x09
  , 0xe2, 0x99, 0xf8, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00
  -- UIDs
  , 0x22, 0x08
  , 0xe4, 0xf7, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00
  -- Indices to username in StringTable
  , 0x2a, 0x06
  , 0x02, 0x00, 0x00, 0x00, 0x00, 0x00 ]

ids :: BS.ByteString
ids = BS.pack [0x9c, 0xb3, 0xaf, 0xde, 0x0a, 0x04, 0x10, 0x16, 0x02, 0x10]

lts :: BS.ByteString
lts = BS.pack [0x98, 0x88, 0x8a, 0xbc, 0x02, 0x78, 0xac, 0x02, 0xf8, 0x05, 0x28, 0x90, 0x03]

lns :: BS.ByteString
lns = BS.pack [0xf4, 0xd4, 0xc6, 0xcf, 0x09, 0xa7, 0x0a, 0xe4, 0x0f, 0xf3, 0x0d, 0xe8, 0x0c, 0x8b, 0x06 ]

-- | Bytes which represent a `Way` from @shrine.osm@.
wey :: BS.ByteString
wey = BS.pack [
  -- PrimitiveGroup field marking this as a Way
  0x1a, 0x2d
  -- id
  , 0x08, 0xfa, 0xd8, 0xfc, 0x6f
  -- keys
  , 0x12, 0x02, 0x04, 0x03
  -- vals
  , 0x1a, 0x02, 0x08, 0x0a
  -- Embedded `Info`
  , 0x22, 0x13
  , 0x08, 0x01, 0x10, 0xb0, 0xba, 0xdf, 0x90, 0x05, 0x18, 0xe6, 0xd8, 0xaa
  , 0x08, 0x20, 0xb9, 0x8f, 0x0d, 0x28, 0x01
  -- Node ids
  , 0x42, 0x09
  , 0xfa, 0x8c, 0x81, 0x8d, 0x12, 0x29, 0x08, 0x30, 0x0d ]

stringTableT :: Assertion
stringTableT = case A.parseOnly stringTable st of
  Left _  -> assertFailure "Couldn't parse StringTable"
  Right t -> assertBool "Damn" . not $ V.null t

denseNodesT :: Assertion
denseNodesT = case A.parseOnly (dense V.empty) dn of
  Left err -> assertFailure err
  Right _  -> pure ()

denseInfoT :: Assertion
denseInfoT = case A.parseOnly stringTable st >>= \t -> A.parseOnly (denseInfo [1..] t) dinf of
  Left err -> assertFailure err
  Right _  -> pure ()

varintT :: BS.ByteString -> Assertion
varintT bs = case A.parseOnly (A.many1 (unzig <$> varint)) bs of
               Left err -> assertFailure err
               Right t  -> length t @?= 6

wayT :: Assertion
wayT = case A.parseOnly stringTable st >>= \t -> A.parseOnly (way t) wey of
  Left err -> assertFailure err
  Right _  -> pure ()

shrineT :: Assertion
shrineT = fileT "test/shrine.osm.pbf" >>= blockT (5, 1, 0)

fileS :: FilePath -> (Int, Int, Int) -> Assertion
fileS fp expected = do
  let bs = blocks $ blobs fp
  nwrs <- runResourceT $ (,,)
          <$> S.length_ (nodes bs)
          <*> S.length_ (ways bs)
          <*> S.length_ (relations bs)
  nwrs @?= expected

diomedeT :: Assertion
diomedeT = fileT "test/diomede.osm.pbf" >>= blockT (510, 74, 1)

tashirojimaT :: Assertion
tashirojimaT = fileT "test/tashirojima.osm.pbf" >>= blockT (4040, 384, 2)

nozakijimaT :: Assertion
nozakijimaT = fileT "test/nozakijima.osm.pbf" >>= blockT (3371, 151, 8)

ajishimaT :: Assertion
ajishimaT = fileT "test/ajishima.osm.pbf" >>= blockT (5118, 325, 1)

--ukuT :: Assertion
--ukuT = fileT "uku.osm.pbf" >>= blockT (8000, 0, 0)

blockT :: (Int, Int, Int) -> Either String Block -> Assertion
blockT _ (Left err) = assertFailure err
blockT (n, w, r) (Right b) = do
  length (_nodes b) @?= n
  length (_ways b) @?= w
  length (_relations b) @?= r

-- | Parse the first data `Blob` from a given file.
fileT :: FilePath -> IO (Either String Block)
fileT f = do
  bites <- BS.readFile f
  case A.parseOnly (header *> blob *> header *> blob) bites of
    Left err -> pure $ Left err
    Right (Blob (Left bs)) -> pure $ A.parseOnly block bs
    Right (Blob (Right (_, bs))) -> pure $ A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs

diomedeRefValues :: Assertion
diomedeRefValues = fileT "test/diomede.osm.pbf" >>= \case
  Left _ -> assertFailure "Couldn't parse the file."
  Right (Block _ _ ((Relation [m0, m1] _ _):_)) -> do
    _mref m0 @?= 32973894
    _mref m1 @?= 4571349198
  Right (Block _ _ _) -> assertFailure "Incorrect relation structure."

ajishimaRefValues :: Assertion
ajishimaRefValues = fileT "test/ajishima.osm.pbf" >>= \case
  Left _ -> assertFailure "Couldn't parse the file."
  Right (Block _ _ rs) -> case filter p rs of
    [] -> assertFailure "Couldn't find the expected Relation."
    (Relation ms _ ts : _) -> do
      length ms @?= 19
      length ts @?= 6
      _mref (head ms) @?= 179786323
      _mref (last ms) @?= 91476273
  where
    p (Relation _ (Just i) _) = _id i == 1341344
    p _                       = False
