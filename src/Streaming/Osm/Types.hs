module Streaming.Osm.Types
  ( -- * Elements
--    Element(..)
    Node(..)
  , Way(..)
  , Relation(..)
  , Info(..)
  -- * Helper Types
  , BlobHeader(..)
  , Blob(..)
  , Block(..)
  ) where

import qualified Data.ByteString as B
import           Data.Int
import qualified Data.Map as M
import           Data.Text (Text)

---

{-}
class Element a where
  info :: a -> Info
  tags :: a -> M.Map Text Text
-}

data Node = Node { _lat   :: Double
                 , _lng   :: Double
                 , _ninfo :: Maybe Info
                 , _ntags :: M.Map B.ByteString B.ByteString
                 } deriving (Eq, Show)

{-}
instance Element Node where
  info = _ninfo
  tags = _ntags
-}
data Way = Way { _nodeRefs :: [Int]
               , _winfo    :: Maybe Info
               , _wtags    :: M.Map Text Text
               } deriving (Eq, Show)

{-}
instance Element Way where
  info = _winfo
  tags = _wtags
-}
data Relation = Relation { _members :: [Member]
                         , _rinfo   :: Info
                         , _rtags   :: M.Map Text Text
                         } deriving (Eq, Show)

{-}
instance Element Relation where
  info = _rinfo
  tags = _rtags
-}
data Member = Member { _mref :: Int, _mrole :: Text } deriving (Eq, Show)

-- | Non-geographic `Element` metadata. The OSM database is a wild place, so
-- many of these fields may be missing for older Elements.
data Info = Info { _id        :: Int
                 , _version   :: Int
                 , _timestamp :: Maybe Int
                 , _changeset :: Maybe Int
                 , _uid       :: Maybe Int
                 , _username  :: Maybe B.ByteString
                 , _visible   :: Maybe Bool
                 } deriving (Eq, Show)

data BlobHeader = BlobHeader { blobType  :: B.ByteString
                             , indexData :: Maybe B.ByteString
                             , datasize  :: Int32 } deriving (Show)

newtype Blob = Blob { bytes :: Either B.ByteString (Int32, B.ByteString) } deriving (Eq, Show)

-- | A group of ~8000 OSM Elements.
data Block = Block { nodes :: [Node]
                   , ways  :: [Way]
                   , relations :: [Relation] } deriving (Eq, Show)
