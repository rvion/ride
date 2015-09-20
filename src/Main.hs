{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)

main :: IO ()
main =
  runSpock 3000 $ spockT id $ do
    get "" $ file "test" "static/index.html"
    get ("echo" <//> var) $ \something ->
      text $ T.concat ["Echo: ", something]