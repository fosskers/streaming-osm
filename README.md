# `streaming-osm`

This library provides the ability to read and process
[OpenStreetMap](http://www.openstreetmap.org/) data via the
[streaming](https://hackage.haskell.org/package/streaming) ecosystem. Since
*streaming* allows for very little RAM overhead despite file size, we can
process very large OSM PBF files just by providing a file path:

```haskell
import           Streaming
import           Streaming.Osm
import qualified Streaming.Prelude as S

-- | Count all nodes.
count :: IO ()
count = do
  len <- runResourceT . S.length_ . nodes . blocks $ blobs "yourfile.osm.pbf"
  print len
```
