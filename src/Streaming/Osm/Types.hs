-- |
-- Module    : Streaming.Osm.Types
-- Copyright : (c) Colin Woodbury, 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

module Streaming.Osm.Types
  ( -- * OpenStreetMap /Elements/
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

-- | An OpenStreetMap /Node/ element. Represents a single point in 2D space.
data Node = Node { _lat   :: Double
                 , _lng   :: Double
                 , _ninfo :: Maybe Info
                 , _ntags :: M.Map B.ByteString B.ByteString
                 } deriving (Eq, Show)

-- | An OpenStreetMap /Way/ element. Made up of `Node`s, and represents
-- some line on the earth, or a Polygon if its first and last Nodes are the same.
data Way = Way { _nodeRefs :: [Int]
               , _winfo    :: Maybe Info
               , _wtags    :: M.Map B.ByteString B.ByteString
               } deriving (Eq, Show)

-- | An OpenStreetMap /Relation/ element. These are logical groups of Nodes, Ways,
-- and other Relations. They can represent large multipolygons, or more abstract
-- non-polygonal objects like bus route networks.
data Relation = Relation { _members :: [Member]
                         , _rinfo   :: Maybe Info
                         , _rtags   :: M.Map B.ByteString B.ByteString
                         } deriving (Eq, Show)

-- | Equivalent to the /member/ tag, found in OSM `Relation`s.
data Member = Member { _mref :: Int, _mtype :: MemType, _mrole :: B.ByteString } deriving (Eq, Show)

-- | Is a `Member` entry referencing a Node, a Way, or a Relation?
data MemType = N | W | R deriving (Eq, Show)

-- | A bridge between the Int-based enum value for `MemType` is protobuf and
-- a more useful Haskell type.
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

-- | Bytes parsed out of an @OSMData@ section. Either non-compressed (Left) or
-- compressed (Right) with its post-decompression size.
newtype Blob = Blob { bytes :: Either B.ByteString (Int, B.ByteString) } deriving (Eq, Show)

-- | A group of ~8000 OSM Elements.
data Block = Block { _nodes :: [Node]
                   , _ways  :: [Way]
                   , _relations :: [Relation] } deriving (Eq, Show)
