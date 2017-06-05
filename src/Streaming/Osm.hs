module Streaming.Osm where

import           Codec.Compression.Zlib (decompress)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Streaming as B
import           Streaming
import           Streaming.Osm.Parser
import           Streaming.Osm.Types
import qualified Streaming.Prelude as S

---

-- | A friendly alias. All `Stream`s in this module are constrained to this
-- type for optimal performance. The opposite, say:
--
-- @
-- nodes :: Monad m => Stream (Of Block) m () -> Stream (Of Node) m ()
-- @
-- will actually run significantly slower.
--
-- After evaluating your `Stream` to some final `RIO`, you can further
-- escape back to `IO` via `runResourceT`.
type RIO = ResourceT IO

-- | Given a `FilePath` to read OSM PBF data from, stream all parsed `Blob`s
-- out of it. A `Blob` is a potentially compressed @ByteString@ that further
-- parse into actual OSM Elements.
blobs :: FilePath -> Stream (Of Blob) RIO ()
blobs = void . A.parsed (header *> blob) . B.readFile

-- | Every `Block` of ~8000 Elements.
blocks :: Stream (Of Blob) RIO () -> Stream (Of Block) RIO ()
blocks = S.concat . S.map f
  where f (Blob (Left bs)) = A.parseOnly block bs
        f (Blob (Right (_, bs))) = A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs

-- TODO: Feels wasteful to be converting between BS types!
-- Can we grab ByteString, convert to Streaming Bytestring, decompress, and
-- then streaming parse?

-- | All OSM `Node`s.
nodes :: Stream (Of Block) RIO () -> Stream (Of Node) RIO ()
nodes = S.concat . S.map _nodes

-- | All OSM `Way`s.
ways :: Stream (Of Block) RIO () -> Stream (Of Way) RIO ()
ways = S.concat . S.map _ways

-- | All OSM `Relation`s.
relations :: Stream (Of Block) RIO () -> Stream (Of Relation) RIO ()
relations = S.concat . S.map _relations
