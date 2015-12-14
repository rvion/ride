{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Test where

import           ClassyPrelude
import           Util

testHandler :: Handler a b
testHandler = do
  State numb <- getState
  liftIO $ print (asString "ok1")
  u <-  db getFirstUser
  [Single a] <-  db get4
  lucid $ h1_ [] $ do
    toS (asString "ok")
    toS numb
    toS (asInt a)
    where
      get4 :: Req [Single  Int]
      get4 = rawSql "select 2 + ?" [toPersistValue (2::Int)]

      getFirstUser :: Req (Maybe (Entity User))
      getFirstUser = selectFirst [] [Desc UserEmail]
