{-# LANGUAGE BinaryLiterals #-}

module Streaming.Osm.Util
  ( key
  , key2
  , foldBytes, foldBytes'
  , breakOn0
  , pairs
  , both
  -- * Helpers for writing the Parser
  , unkey
  ) where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Word

---

to16 :: Word8 -> Word16
to16 = fromIntegral

to8 :: Word16 -> Word8
to8 = fromIntegral

unkey :: Word8 -> Word8 -> Either (Word8, Word8) Word8
unkey f w = case (to8 $ shiftR smash 7, to8 $ smash .&. 0b01111111) of
  (0, r) -> Right r
  (l, r) -> Left (setBit r 7, l)
  where smash = shift (to16 f) 3 .|. to16 w

-- | Discover a field's number and /Wire Type/. The wire type is expected to
-- be a value from 0 to 5. The field number itself can probably be any varint,
-- although in practice these are in `Word8` range.
--
-- The results are left as numbers, since pattern matching on those should
-- be faster.
key :: (Num t, Bits t) => t -> (t, t)
key w = (shiftR w 3, w .&. 0b00000111)
{-# INLINABLE key #-}

-- | For the case when two bytes denote the field number and /Wire Type/. We
-- know that for OSM data, the highest field number is 34. Encoding 34 with any
-- wire type takes 2 bytes, so we know we'll never need to check for more.
key2 :: Word8 -> Word8 -> (Word16, Word16)
key2 w1 w0 = key $ shift (to16 w0) 7 .|. to16 (clearBit w1 7)

-- | Fold a `BS.ByteString` into some number type, according to the special
-- rules outlined in `groupBytes`.
foldBytes :: (Num a, Bits a) => BS.ByteString -> a
foldBytes = BS.foldr' (\w acc -> shift acc 7 .|. clearBit (fromIntegral w) 7) zeroBits
{-# INLINABLE foldBytes #-}

-- | Like the above, but takes a `Word8` as the initial accumulator. This is
-- useful when parsing Varints with `Data.Attoparsec.ByteString.takeWhile`.
foldBytes' :: (Num a, Bits a) => BS.ByteString -> Word8 -> a
foldBytes' bs b = BS.foldr' (\w acc -> shift acc 7 .|. clearBit (fromIntegral w) 7) (fromIntegral b) bs
{-# INLINABLE foldBytes' #-}

-- | `words` for `Int`, where 0 is the whitespace. Implementation adapted
-- from `words`.
breakOn0 :: [Int] -> [[Int]]
breakOn0 [] = []
breakOn0 ns = xs : breakOn0 ys
  where (xs, (0 : ys)) = span (/= 0) ns

-- | A sort of "self-zip", forming pairs from every two elements in a list.
-- Assumes that the list is of even length.
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:y:zs) = (x,y) : pairs zs

-- | Apply a function to both elements of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)

-- | Break up a `BS.ByteString` that was parsed with wire-type 2
-- (Length-delimited). These follow the pattern @tagByte byteCount bytes@,
-- where we've parsed @byteCount@ and done an attoparsec @take@ on the @bytes@.
-- These bytes could be a packed repeated field of varints, meaning they could
-- all have different byte lengths.
--
-- This function uses the rules described
-- <https://developers.google.com/protocol-buffers/docs/encoding here> for
-- determining when to accumulate the current byte, or to consider it the first
-- byte of the next value. In short, the rule is:
--
--   1. If the MSB of a byte is 1, then expect at least the next byte to belong to this value.
--   2. If the MSB of a byte is 0, we're at the end of the current accumulating value (or
--      at the first byte of a single byte value).
--groupBytes :: BS.ByteString -> [BS.ByteString]
--groupBytes = BS.groupBy (\a _ -> testBit a 7)
