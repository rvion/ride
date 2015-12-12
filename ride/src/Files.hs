{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}

module Files where

import System.Directory.Tree

import Data.Aeson
import Data.Aeson.Types
-- import Control.Lens
import GHC.Generics
import GHC.IO.Exception
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import System.IO.Error (userError)

import "cryptohash" Crypto.Hash

sha1 :: ByteString -> Digest SHA1
sha1 = hash


-- http://docs.webix.com/datatree__data_formats.html
-- [
--     { id:"1", open:true, value:"Toyota", data:[
--         { id:"1.1", value:"Avalon" },
--         { id:"1.2", value:"Camry" },
--         { id:"1.3", value:"Corolla" }
--     ]},
--     { id:"2", value:"Skoda", data:[
--         { id:"2.1", value:"Octavia" },
--         { id:"2.2", value:"Superb" }
--     ]}
-- ]
instance ToJSON (DirTree String) where
  toJSON (Failed name err) = object ["value" .= name, "err" .= (show err)]
  toJSON (Dir name contents) = object ["value" .= name, "data" .= contents]
  toJSON (File name file) = object ["value" .= name, "content" .= file]

instance FromJSON (DirTree String) where
    parseJSON (Object v) =
      let constr  = (.: "type") :: (Object -> Parser String)
      in case parse constr v of
          Success "Failed" -> Failed <$> v .: "value" <*> (userError <$> v .: "err")
          Success "Dir" -> Dir <$> v .: "value" <*> v .: "data"
          Success "File" -> File <$> v .: "value" <*> v .: "content"
          Error "ok" -> error "ok"
    -- No need to provide a parseJSON implementation.

fph :: IO (AnchoredDirTree String)
fph = readDirectoryWith (\fp -> (show.digestToHexByteString . sha1) <$> BS.readFile fp ) "src/"

fp :: IO (AnchoredDirTree String)
fp = readDirectory "src/"

fp' :: IO (DirTree String)
fp' = dirTree <$> fp

fph' :: IO (DirTree String)
fph' = dirTree <$> fph

-- dirjson :: IO LBS.ByteString
-- dirjson = encode <$> fp'
