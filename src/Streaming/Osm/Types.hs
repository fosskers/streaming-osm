module Streaming.Osm.Types where

import qualified Data.Map as M
import           Data.Text (Text)

---

class Element a where
  info :: a -> Info
  tags :: a -> M.Map Text Text

data Node = Node { _lat   :: Double
                 , _lng   :: Double
                 , _ninfo :: Info
                 , _ntags :: M.Map Text Text
                 } deriving (Eq, Show)

instance Element Node where
  info = _ninfo
  tags = _ntags

data Way = Way { _nodeRefs :: [Int]
               , _winfo    :: Info
               , _wtags    :: M.Map Text Text
               } deriving (Eq, Show)

instance Element Way where
  info = _winfo
  tags = _wtags

data Relation = Relation { _members :: [Member]
                         , _rinfo   :: Info
                         , _rtags   :: M.Map Text Text
                         } deriving (Eq, Show)

instance Element Relation where
  info = _rinfo
  tags = _rtags

data Member = Member { _mref :: Int, _mrole :: Text } deriving (Eq, Show)

-- | Non-geographic `Element` metadata. The OSM database is a wild place, so
-- many of these fields may be missing for older Elements.
data Info = Info { _id        :: Int
                 , _version   :: Int
                 , _timestamp :: Maybe Int
                 , _changeset :: Maybe Int
                 , _uid       :: Maybe Int
                 , _username  :: Maybe Text
                 , _visible   :: Maybe Bool
                 } deriving (Eq, Show)
