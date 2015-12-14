{-# LANGUAGE OverloadedStrings #-}
module DB where

import           Control.Monad
import           Data.Aeson
import           Data.Map      (Map)
import           Data.Thyme
-- import qualified Data.Map as M

type Name = String
type DB = Map Name Life

data Position = Position
  { title     :: String
  , company   :: String
  , starttime :: String
  , endtime   :: String
  } deriving (Show, Read)

instance ToJSON Position where
  toJSON pos = object
    [ "title"     .= title pos
    , "company"   .= company pos
    , "starttime" .= starttime pos
    , "endtime"   .= endtime pos
    ]

instance FromJSON Position where
  parseJSON (Object v) = Position
    <$> v .: "title"
    <*> v .: "company"
    <*> v .: "starttime"
    <*> v .: "endtime"
  parseJSON _ = mzero

data Life = Life
  { name   :: String
  , events :: [Position]
  } deriving (Show, Read)

instance ToJSON Life where
  toJSON life = object
    [ "value"     .= name life
    , "content"   .= events life
    ]

instance FromJSON Life where
  parseJSON (Object v) = Life
    <$> v .: "name"
    <*> v .: "life"
  parseJSON _ = mzero

    -- A non-Object value is of the wrong type, so fail.

