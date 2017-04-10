{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
import           Data.Int
import           Data.Word
import           Streaming.Osm.Util
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Util"
    [ testGroup "foldBytes"
      [ testCase "3" $ foldBytes @Int (BS.pack [0x03]) @?= 3
      , testCase "124" $ foldBytes @Int32 (BS.pack [0x7c]) @?= 124
      , testCase "150" $ foldBytes @Int (BS.pack [0x96, 0x01]) @?= 150
      , testCase "270" $ foldBytes @Int (BS.pack [0x8e, 0x02]) @?= 270
      , testCase "86942" $ foldBytes @Int (BS.pack [0x9e, 0xa7, 0x05]) @?= 86942
      ]
    , testGroup "key"
      [ testCase "08" $ key @Word8 0x08 @?= (1, 0)
      , testCase "51" $ key @Word8 0x51 @?= (10, 1)
      , testCase "12" $ key @Word8 0x12 @?= (2, 2)
      , testCase "1a" $ key @Word8 0x1a @?= (3, 2)
      , testCase "22" $ key @Word8 0x22 @?= (4, 2)
      , testCase "55" $ key @Word8 0x55 @?= (10, 5)
      ]
    , testGroup "key2"
      [ testCase "82 01" $ key2 0x82 0x01 @?= (16, 2)
      , testCase "92 02" $ key2 0x92 0x02 @?= (34, 2)
      ]
    , testGroup "unkey"
      [ testCase "34 string" $ unkey 34 2 @?= Left (0x92, 0x02)
      , testCase "16 string" $ unkey 16 2 @?= Left (0x82, 0x01)
      , testCase "01 varint" $ unkey 01 0 @?= Right 0x08
      , testCase "02 string" $ unkey 02 2 @?= Right 0x12
      , testCase "03 string" $ unkey 03 2 @?= Right 0x1a
      , testCase "04 string" $ unkey 04 2 @?= Right 0x22
      , testCase "10 64bit"  $ unkey 10 1 @?= Right 0x51
      , testCase "10 32bit"  $ unkey 10 5 @?= Right 0x55
      ]
--    , testGroup "groupBytes"
--      [ testCase "03 8E 02 9E A7 05" groupBytesT
--      ]
    ]
  ]

--groupBytesT :: Assertion
--groupBytesT = nums @?= [3, 270, 86942]
--  where nums = map (foldBytes @Int) $ groupBytes (BS.pack [0x03, 0x8E, 0x02, 0x9E, 0xA7, 0x05])
