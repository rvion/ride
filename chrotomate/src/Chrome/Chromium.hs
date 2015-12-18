{-# LANGUAGE OverloadedStrings #-}
module Chrome.Chromium where

import           Jetpack

-- 1: Start Chrome with Remote debugging protocol enabled
-- see (https://developer.chrome.com/devtools/docs/debugger-protocol) for doc
startChromeCmd :: String
startChromeCmd = unwords
    [ "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome"
    , "--remote-debugging-port=9160"
    , "http://localhost:9160"
    , "http://chromium.org"
    ]

startChrome :: IO CtrlThreadId
startChrome = ctrl_forkIO (env_system startChromeCmd >>= print )

-- 2: Get Linkedin Page ID
getLinkedinPageId :: MonadIO m => m (Maybe JsValue)
getLinkedinPageId = getWebsitePageId "https://www.linkedin.com/"

getWebsitePageId :: MonadIO m => TText -> m (Maybe JsValue)
getWebsitePageId website = do
    initReq <- liftIO $ http_parseUrl "http://localhost:9222/json"
    manager <- liftIO $ http_newManager http_tlsManagerSettings
    let req = initReq
            & set_http_method "POST"
            & set_http_requestHeaders [ ("Accept", "application/json")]
    resp <- liftIO $ get_http_responseBody <$> http_httpLbs req manager
    -- let s1 = resp ^. lens__Array ^.. lens_folded . filtered (\o -> T.isPrefixOf website $ fromJust (o ^? key "url" . _String))
    --     s2 = s1 ^. lens_to head ^? js_key "id"
    -- liftIO $ print s1
    -- return s2
    return undefined

-- connectPage :: String -> IO ()
-- connectPage pageID = withSocketsDo $ WS.runClient "localhost" 9222 ("/devtools/page/" <> pageID) linkedinScript
-- searchName name = fillInput "#main-search-box" name
-- Chrome Remote debugging protocol

fillInput :: String -> String -> LbsByteString
fillInput inpt txt = jsEval (concat ["$(",str inpt,").value(",str txt,");"])

jsEval :: String -> LbsByteString
jsEval code = js_encode $ js_object ["command" .= ("evaluate_javascript"::String), "data" .= code]

-- Helpers
str :: String -> String
str a = concat ["\"", a, "\""]
