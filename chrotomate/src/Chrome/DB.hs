{-# LANGUAGE OverloadedStrings #-}
module Chrome.DB where

import           JetPack
-- import           Control.Monad
-- import           Data.Aeson ()
-- import Data.Aeson.Types (ToJSON, FromJSON)
-- type PLOP = ToJSON
-- import           Data.Map      (Map)
-- import           Data.Thyme
-- import qualified Data.Map as M

type Name = String
type DB = MapMap Name Life

data Position = Position
  { title     :: String
  , company   :: String
  , starttime :: String
  , endtime   :: String
  } deriving (Show, Read)

instance ToJSON Position where
  toJSON pos = js_object
    [ "title"     .= title pos
    , "company"   .= company pos
    , "starttime" .= starttime pos
    , "endtime"   .= endtime pos
    ]

instance FromJSON Position where
  parseJSON (JsObject v) = Position
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
  toJSON life = js_object
    [ "value"     .= name life
    , "content"   .= events life
    ]

instance FromJSON Life where
  parseJSON (JsObject v) = Life
    <$> v .: "name"
    <*> v .: "life"
  parseJSON _ = mzero

    -- A non-Object value is of the wrong type, so fail.

