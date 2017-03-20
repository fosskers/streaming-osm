module Streaming.Osm.Util where

import           Data.Bits
import qualified Data.ByteString as BS

---

-- | Fold a `BS.ByteString` into some number type, according to the special
-- rules outlined in `groupBytes`.
foldBytes :: (Num a, Bits a) => BS.ByteString -> a
foldBytes = BS.foldr' (\w acc -> shift acc 7 .|. clearBit (fromIntegral w) 7) zeroBits

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
groupBytes :: BS.ByteString -> [BS.ByteString]
groupBytes = BS.groupBy (\a _ -> testBit a 7)
