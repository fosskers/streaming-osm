module Main where

import           Control.Monad.Trans.Resource (runResourceT)
import           Criterion.Main
import           Streaming.Osm
import qualified Streaming.Prelude as S

---

main :: IO ()
main = defaultMain
  [ bgroup "Counting Nodes"
    [ bench "Uku Seq" $ nfIO (runResourceT .  S.length_ . nodes . blocks $ blobs "test/uku.osm.pbf")
   -- , bench "Uku Par" $ nfIO (runResourceT . blocks' (S.length_ . nodes) $ blobs "test/uku.osm.pbf")
    -- , bench "Van Seq" $ nfIO (runResourceT .  S.length_ . nodes . blocks $ blobs "test/vancouver.osm.pbf")
   --  , bench "Van Par" $ nfIO (runResourceT . blocks' (S.length_ . nodes) $ blobs "test/vancouver.osm.pbf")
    ]
  , bgroup "Counting Ways"
    [ bench "Uku" $ nfIO (runResourceT .  S.length_ . ways . blocks $ blobs "test/uku.osm.pbf")
    ]
  , bgroup "Counting Relations"
    [ bench "Uku" $ nfIO (runResourceT .  S.length_ . relations . blocks $ blobs "test/uku.osm.pbf")
    ]
  ]
