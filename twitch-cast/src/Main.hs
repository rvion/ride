module Main where
-- import Data.Functor.Identity (Indentity)
import Jetpack
import HLS
import Types
import Lucid

-- sk = ansi_setSGR [ansi_mk'Vivid ansi_mk'Blue]

main :: IO ()
main = do
  putStrLn "starting webserver"
  let ctx  = Ctx {streams = [d2']}
  spock_runSpock 3000 $ spock_spockT (runM ctx) $ do

    spock_get "ok" $ do
      (Ctx{..}) <- lift (M trans_get)
      spock_json $js_object ["ok" .= (33::Int)]

    spock_get ("test" <//> spock_var) $ \v -> do
      liftIO (startVlc demoHLSParams)
      spock_text  v
  return ()






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
    env_hPutStrLn env_stderr l

startVlc :: HLSParams -> IO ()
startVlc hlsParams = do
  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- env_createProcess (vlcCmd hlsParams)
  case mb_stdout_hdl of
    Nothing -> print "err"
    Just stdout_hdl -> do
      forkIO $ doStuffWithVLCOutput stdout_hdl
      print "Listening"
  return ()

vlcCmd :: HLSParams -> EnvCreateProcess
vlcCmd hlsParam = env_proc "yes" [vlcCmdParam hlsParam]
  & set_env_std_in env_mk'CreatePipe
  & set_env_std_out env_mk'CreatePipe
  & set_env_std_err env_mk'Inherit

a = env_createProcess
runM :: Ctx -> M a -> IO a
runM ctx = (\x -> trans_evalStateT x ctx) . unM


showStream :: Html ()
showStream = do
  h1_ [] "Twitch-Streams"
  table_ $ do
    (tr_ (td_ (p_ "Hello, World!")))
    (tr_ (td_ (p_ "Hello, World!")))
    (tr_ (td_ (p_ "Hello, World!")))
