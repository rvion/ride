{-# LANGUAGE OverloadedStrings #-}
module Chrome.Chromium where

import JetPack
-- import           Control.Concurrent     (ThreadId, forkIO)
-- import           Control.Monad          (forever, unless)
-- import           Control.Monad.IO.Class (MonadIO)
-- import           Control.Monad.Trans    (liftIO)

-- import           Network.HTTP.Conduit
-- import           Network.Socket         (withSocketsDo)
-- import qualified Network.WebSockets     as WS

-- import           Data.Text              (Text)
-- import qualified Data.Text              as T
-- import qualified Data.Text.IO           as T

-- import           System.Exit            (ExitCode)
-- import           System.Process         (system)

-- import           Control.Lens           (filtered, folded, to, (^.), (^..), (^?))
-- import           Data.Aeson
-- import           Data.Aeson.Lens        (key, _Array, _String)
-- import qualified Data.ByteString.Lazy   as LBS
-- import           Data.List              (isPrefixOf)
-- import           Data.Maybe             (fromJust)
-- import           Data.Monoid

type LBS = LBS.ByteString

-- 1: Start Chrome with Remote debugging protocol enabled
-- see (https://developer.chrome.com/devtools/docs/debugger-protocol) for doc
startChromeCmd :: String
startChromeCmd = unwords
    [ "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome"
    , "--remote-debugging-port=9160"
    , "http://localhost:9160"
    , "http://chromium.org"
    ]

startChrome :: IO ThreadId
startChrome = forkIO (system startChromeCmd >>= print )

-- 2: Get Linkedin Page ID
getLinkedinPageId :: MonadIO m => m (Maybe Value)
getLinkedinPageId = getWebsitePageId "https://www.linkedin.com/"

getWebsitePageId :: MonadIO m => T.Text -> m (Maybe Value)
getWebsitePageId website = do
    initReq <- liftIO $ parseUrl "http://localhost:9222/json"
    manager <- liftIO $ newManager tlsManagerSettings
    let req = initReq { method = "POST", requestHeaders =  [ ("Accept", "application/json")] }
    resp <- liftIO $ responseBody <$> httpLbs req manager
    let s1 = resp ^. _Array ^.. folded . filtered (\o -> T.isPrefixOf website $ fromJust (o ^? key "url" . _String))
        s2 = s1 ^. to head ^? key "id"
    liftIO $ print s1
    return s2

-- connectPage :: String -> IO ()
-- connectPage pageID = withSocketsDo $ WS.runClient "localhost" 9222 ("/devtools/page/" <> pageID) linkedinScript
-- searchName name = fillInput "#main-search-box" name
-- Chrome Remote debugging protocol

fillInput :: String -> String -> LBS
fillInput inpt txt = jsEval (concat ["$(",str inpt,").value(",str txt,");"])

jsEval :: String -> LBS
jsEval code = encode $ object ["command" .= ("evaluate_javascript"::String), "data" .= code]

-- Helpers
str :: String -> String
str a = concat ["\"", a, "\""]













