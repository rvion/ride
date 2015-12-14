{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Data.Time
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateCore"] [persistLowerCase|
Session
  validUntil UTCTime
  userId UserId
  deriving Show

Link
  url Text
  hash ByteString
  createdAt UTCTime
  UniqueHash hash
  deriving Show

User json
  name Text
  password Text
  salt Text
  email Text
  isAuthor Bool
  isAdmin Bool
  UniqueUsername name
  UniqueEmail email
  deriving Show

Post
  title Text
  date UTCTime
  content Text
  deriving Show

PostTags
  postId PostId
  tagId TagId
  UniquePostTag postId tagId
  deriving Show

Tag
  tag Text
  UniqueTag tag
  deriving Show

Comment
  author Text
  email Text Maybe
  content Text
  post PostId
  deriving Show
|]
