{-# LANGUAGE NamedFieldPuns, RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Lib ( someFunc ) where
import Jetpack

data Ctx = Ctx
  { ctxGithubToken :: Maybe String
  , ctxHostName :: String
  }

newtype M a = M
  { unM :: TransStateT Ctx IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

runM :: Ctx ->  M a -> IO a
runM c = (\x -> trans_evalStateT x c) . unM

webserver :: Ctx -> IO ()
webserver c = spock_runSpock 3000 $ spock_spockT (runM c) $ do
  spock_get ("auth/resp") $ spock_text "name"
  spock_get ("auth") $ do
    Ctx{..} <- lift (M trans_get)
    let
      -- authReq = AuthReq
      --   { arClientId = name
      --   , arRedirectUri = ctxHostName ++ "auth/resp"
      --   , arScope = "repo, user"
      --   , arState = "toto"
      --   }
      q = http_renderQuery True
        [ ("client_id", Just "444f8ad432847b2de393")
        , ("redirect_uri", Just $ fromString $ ctxHostName ++ "auth/resp")
        , ("scope", Just "rJust $ epo, user")
        , ("state", Just "toto")
        ]
    spock_redirect $ "https://github.com/login/oauth/authorize" <> (t_decodeUtf8 q)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data AuthReq = AuthReq
  { arClientId :: String
  , arRedirectUri :: String
  , arScope :: String
  , arState :: String
  }

instance ToJSON AuthReq where
  toJSON AuthReq{..} = js_object
    [ "client_id" .= arClientId
    , "redirect_uri" .= arRedirectUri
    , "scope" .= arScope
    , "state" .= arState
    ]

data AuthResp = AuthResp
  { arespAccessToken :: String
  , arespScope :: String
  , arespTokenType :: String
  }

instance FromJSON AuthResp where
  parseJSON (JsObject a) = AuthResp
    <$> a .: "access_token"
    <*> a .: "scope"
    <*> a .: "token_type"

-- rvion
type EndPoint = String

-- getAuth = do
--   r <- post GET https://github.com/login/oauth/authorize

post :: ToJSON a => EndPoint -> a -> IO LbsByteString
post e obj = do
  request <- http_parseUrl e
  let
    req = request
      & set_http_requestHeaders [ (http_hAccept,  "application/json") ]
      & set_http_requestBody (http_mk'RequestBodyLBS $ js_encode obj)

  manager <- http_newManager http_tlsManagerSettings
  r <- trans_runResourceT (http_httpLbs request manager)
  return $ get_http_responseBody r
  -- return response
  -- return ()
    -- http_httpLbs
    -- get_http_responseBody response $$+- cb_sinkFile "google.html"

-- https://developer.github.com/v3/repos/statuses/
-- https://developer.github.com/v3/oauth/

u = undefined
