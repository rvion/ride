
module Main where

import           App
import           ClassyPrelude
-- import           Data.Maybe               (fromMaybe)
import           Network.Wai              (Middleware)
import           Network.Wai.Handler.Warp (Port, run)
import           System.Environment       (lookupEnv)
import           Util                     (spockAsApp)

main :: IO ()
main = do
  mbport <- lookupEnv "PORT"
  let port = fromMaybe 8080 (mbport >>= readMay)
  start port application


start :: Port -> IO Middleware -> IO ()
start port mw = do
  putStrLn ("App is running on port " ++ tshow port)
  app <- spockAsApp mw
  run port app
