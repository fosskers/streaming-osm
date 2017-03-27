{-# LANGUAGE BinaryLiterals #-}

module Streaming.Osm.Util where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Word

---

-- | Discover a field's number and /Wire Type/. The wire type is expected to
-- be a value from 0 to 5. The field number itself can probably be any varint,
-- although in practice these are in `Word8` range.
--
-- The results are left as `Word8`, since pattern matching on those should
-- be faster.
key :: Word8 -> (Word8, Word8)
key w = (shiftR w 3, w .&. 0b00000111)

-- | Fold a `BS.ByteString` into some number type, according to the special
-- rules outlined in `groupBytes`.
foldBytes :: (Num a, Bits a) => BS.ByteString -> a
foldBytes = BS.foldr' (\w acc -> shift acc 7 .|. clearBit (fromIntegral w) 7) zeroBits
{-# INLINABLE foldBytes #-}

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

{-
Bytes: 0x03, 0x8E, 0x02, 0x9E, 0xA7, 0x05

03        // 0000 0011
8E 02     // 1000 1110 0000 0010
9E A7 05  // 1001 1110 1010 0111 0000 0101

270: 100001110
86942: 10101001110011110
1424457998: 10101001110011110 00000100001110

With `break`, the Word8 that passes the test is included in the remainder.
With `breakEnd`, the opposite is true.

Solution? Write a parser for this?

Either way, it seems that `groupBy` does not behave as I expected.
`groupBytes` is probably not want I want to do in general either, since a
given packed field could be very long. Is it best to keep it as a
ByteString? Stream it? Hm.
-}
