module Streaming.Osm.Types
  ( -- * Elements
--    Element(..)
    Node(..)
  , Way(..)
  , Relation(..)
  , Info(..)
  , Member(..)
  , MemType(..), memtype
  -- * Helper Types
  , Blob(..)
  , Block(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M

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
               , _wtags    :: M.Map B.ByteString B.ByteString
               } deriving (Eq, Show)

{-}
instance Element Way where
  info = _winfo
  tags = _wtags
-}
data Relation = Relation { _members :: [Member]
                         , _rinfo   :: Maybe Info
                         , _rtags   :: M.Map B.ByteString B.ByteString
                         } deriving (Eq, Show)

{-}
instance Element Relation where
  info = _rinfo
  tags = _rtags
-}
data Member = Member { _mref :: Int, _mtype :: MemType, _mrole :: B.ByteString } deriving (Eq, Show)

data MemType = N | W | R deriving (Eq, Show)

memtype :: Int -> MemType
memtype 0 = N
memtype 1 = W
memtype 2 = R

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

newtype Blob = Blob { bytes :: Either B.ByteString (Int, B.ByteString) } deriving (Eq, Show)

-- | A group of ~8000 OSM Elements.
data Block = Block { _nodes :: [Node]
                   , _ways  :: [Way]
                   , _relations :: [Relation] } deriving (Eq, Show)
