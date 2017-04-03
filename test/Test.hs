{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
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
      , testCase "150" $ foldBytes @Int (BS.pack [0x96, 0x01]) @?= 150
      , testCase "270" $ foldBytes @Int (BS.pack [0x8e, 0x02]) @?= 270
      , testCase "86942" $ foldBytes @Int (BS.pack [0x9e, 0xa7, 0x05]) @?= 86942
      ]
    , testGroup "key"
      [ testCase "08" $ key @Word8 0x08 @?= (1, 0)
      , testCase "12" $ key @Word8 0x12 @?= (2, 2)
      , testCase "1a" $ key @Word8 0x1a @?= (3, 2)
      , testCase "22" $ key @Word8 0x22 @?= (4, 2)
      ]
    , testGroup "key2"
      [ testCase "82 01" $ key2 0x82 0x01 @?= (16, 2)
      ]
--    , testGroup "groupBytes"
--      [ testCase "03 8E 02 9E A7 05" groupBytesT
--      ]
    ]
  ]

--groupBytesT :: Assertion
--groupBytesT = nums @?= [3, 270, 86942]
--  where nums = map (foldBytes @Int) $ groupBytes (BS.pack [0x03, 0x8E, 0x02, 0x9E, 0xA7, 0x05])
