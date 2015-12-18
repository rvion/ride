module Main where

import Jetpack
import Lucid

newtype M a = M
  { unM :: TransStateT Ctx IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

data Ctx = Ctx
  { streams :: [TwitchStream]
  }

data HLSParams = HLSParams
  { seg_len :: String
  , index_path :: String
  , ts_url :: String
  , ts_path :: String
  }

demoHLSParams = HLSParams
  { seg_len :: String
  , index_path :: String
  , ts_url :: String
  , ts_path :: String
  }

vlcCmd :: HLSParams -> String
vlcCmd HLSParams{..} = filter (/= ' ') $ concat
  [ "#std {{"
  , "    access = livehttp {{"
  , "        seglen = {",seg_len,"},"
  , "        delsegs = true,"
  , "        numsegs = 10,"
  , "        index = {",index_path,"},"
  , "        index-url = {",ts_url,"}"
  , "    }},"
  , "    mux = ts{{"
  , "        use-key-frames"
  , "    }},"
  , "    dst = {",ts_path,"}"
  ]

cc :: HLSParams -> EnvCreateProcess
cc hlsParam = env_proc "vlc" [vlcCmd hlsParam]
  & set_env_std_in env_mk'Inherit

a = env_createProcess
runM :: Ctx -> M a -> IO a
runM ctx = (\x -> trans_evalStateT x ctx) . unM

main :: IO ()
main = do
  putStrLn "hello world"
  let ctx  = Ctx {streams = [d2']}
  spock_runSpock 3000 $ spock_spockT (runM ctx) $ do

    spock_get "ok" $ do
      (Ctx{..}) <- lift (M trans_get)
      spock_json $js_object ["ok" .= (33::Int)]

    spock_get ("test" <//> spock_var) $ \v -> do
      spock_text  v
  return ()


-- a :: HtmlT () ()
-- a = div [] $ do
--   h1_ [] "ko"
--   h2_ [] "good"
-----------------

data HLSProxy = HLSProxy
  { url :: String
  , ready :: Bool
  } deriving (Show)

data TwitchStream = TwitchStream
  { tsId :: Int
  , channel :: String
  , quality :: String
  , proxy :: Maybe HLSProxy
  } deriving (Show)


u = undefined

instance ToJSON HLSProxy where
  toJSON HLSProxy{..} = js_object
    [ "ready" .= ready
    , "indexUrl" .= url
    ]

instance FromJSON HLSProxy where
  parseJSON (JsObject v) = HLSProxy
    <$> v .: "indexUrl"
    <*> v .: "ready"

instance ToJSON TwitchStream where
  toJSON TwitchStream{..} = js_object
    [ "id" .= tsId
    , "channel" .= channel
    , "quality" .= quality
    , "proxy" .= proxy
    ]
instance FromJSON TwitchStream where
  parseJSON (JsObject v) = TwitchStream
    <$> v .: "id"
    <*> v .: "channel"
    <*> v .: "quality"
    <*> v .: "proxy"

-----------------

demoStream :: Maybe TwitchStream
demoStream = js_decode d2

pp1 :: Maybe HLSProxy
pp1 = js_decode p1

p1 = "{\"indexUrl\": \"http://54.171.4.111/video/lirik/best/stream.m3u8\", \"ready\": true}"

c = js_decode' d1
d1 = "{\"url\": \"twitch.tv/geers_art\", \"quality\": \"best\", \"channel\": \"geers_art\", \"id\": 4, \"proxy\": null}"
d1' = TwitchStream
  { tsId = 4
  , channel = "geers_art"
  , quality = "best"
  , proxy = Nothing
  }

d2 = "{\"url\": \"twitch.tv/lirik\", \"quality\": \"best\", \"channel\": \"lirik\", \"id\": 5, \"proxy\": {\"indexUrl\": \"http://54.171.4.111/video/lirik/best/stream.m3u8\", \"ready\": true}}"
d2' = TwitchStream
  { tsId = 4
  , channel = "geers_art"
  , quality = "best"
  , proxy = Just $ HLSProxy
    { ready = True
    , url = "http://54.171.4.111/video/lirik/best/stream.m3u8"
    }
  }

jsToS :: ToJSON a => a -> LbsByteString
jsToS = js_encode . toJSON

printJs :: ToJSON a => a -> IO ()
printJs x = lbs_putStr (jsToS x) >> putStrLn ""
