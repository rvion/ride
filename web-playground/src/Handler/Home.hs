module Handler.Home where

import           ClassyPrelude
import           Route
import           Util

indexHandler :: Handler a b
indexHandler =  page $ do
  h1_ "Demo"
  a_ [href_ (renderRoute userR)] "User"
  br_ []
  a_ [href_ ( renderRoute addR 313 3)] "Add"


