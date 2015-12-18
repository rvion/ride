module Main where

import Jetpack
import HLS
import Types
import Lucid

newtype M a = M
  { unM :: TransStateT Ctx IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

data Ctx = Ctx
  { streams :: [TwitchStream]
  }

doStuffWithVLCOutput :: EnvHandle -> IO ()
doStuffWithVLCOutput h = -- do
  forever $ do
    l <- env_hGetLine h
    putStrLn l


demoG :: IO ()
demoG = do
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- env_createProcess (vlcCmd demoHLSParams)
  case mb_stdout_hdl of
    Nothing -> print "err"
    Just stdout_hdl -> do
      forkIO $ doStuffWithVLCOutput stdout_hdl
      print "Listening"
  return ()

vlcCmd :: HLSParams -> EnvCreateProcess
vlcCmd hlsParam = env_proc "vlc" [vlcCmdParam hlsParam]
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
