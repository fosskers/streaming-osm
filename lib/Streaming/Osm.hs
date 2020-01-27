-- |
-- Module    : Streaming.Osm
-- Copyright : (c) Azavea, 2017 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- This library provides the ability to read and process <http://www.openstreetmap.org/ OpenStreetMap>
-- data via the <https://hackage.haskell.org/package/streaming streaming> ecosystem. Since /streaming/
-- allows for very little RAM overhead despite file size, we can process very large OSM PBF files
-- just by providing a file path:
--
-- @
-- import           Streaming
-- import           Streaming.Osm
-- import qualified Streaming.Prelude as S
--
-- -- | Count all nodes.
-- count :: IO ()
-- count = do
--   len <- runResourceT . S.length_ . nodes . blocks $ blobs "yourfile.osm.pbf"
--   print len
-- @

module Streaming.Osm
  (
    -- * Streams
    blobs
  , blocks
  , nodes
  , ways
  , relations
    -- * Util
  , RIO
  ) where

import           Codec.Compression.Zlib (decompress)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Streaming as Q
import           Streaming
import           Streaming.Osm.Internal.Parser
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
blobs = void . A.parsed (header *> blob) . Q.readFile

-- | Every `Block` of ~8000 Elements.
blocks :: Stream (Of Blob) RIO () -> Stream (Of Block) RIO ()
blocks = S.concat . S.map f
  where f (Blob (Left bs)) = A.parseOnly block bs
        f (Blob (Right (_, bs))) = A.parseOnly block . BL.toStrict . decompress $ BL.fromStrict bs

-- | All OSM `Node`s.
nodes :: Stream (Of Block) RIO () -> Stream (Of Node) RIO ()
nodes = S.concat . S.map _nodes

-- | All OSM `Way`s.
ways :: Stream (Of Block) RIO () -> Stream (Of Way) RIO ()
ways = S.concat . S.map _ways

-- | All OSM `Relation`s.
relations :: Stream (Of Block) RIO () -> Stream (Of Relation) RIO ()
relations = S.concat . S.map _relations
