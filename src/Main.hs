{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock (runSpock,spockT,middleware, file, get, var, text)
import Control.Monad ()
import qualified Data.Text as T
import Data.Text (Text)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.Aeson ()
import System.Process ()

main :: IO ()
main =
  runSpock 3000 $ spockT id $ do
    middleware $ staticPolicy (addBase "static/")
    get "" $ file "test" "static/index.html"
    -- post "save" $ json ok
    get ("echo" <//> var) $ \something ->
      text $ T.concat ["Echo: ", something]





installGitWebUI = "curl https://raw.githubusercontent.com/alberthier/git-webui/master/install/installer.sh | bash"