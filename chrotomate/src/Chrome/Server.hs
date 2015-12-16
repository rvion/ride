{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Chrome.Server where

import           JetPack
import           Chrome.Command
import           Chrome.DB
-- import Control.Monad.Trans.State (StateT)
data Ctx = Ctx
  { ctxDB     :: DB
  , ctxDbPath :: FilePath
  , ctxConn   :: WsConnection
  }

type M a = TransStateT Ctx IO a

webserver ::Ctx -> IO ()
webserver _ctx = do
  port <- maybe 3000 read <$> env_lookupEnv "PORT"
  spock_runSpock port $ spock_spockT (runM _ctx) ride

runM :: Ctx -> M a -> IO a
runM _ctx = flip trans_evalStateT _ctx

-- ride :: SpockSpockT (StateT Ctx IO) ()
ride = undefined
-- ride = do
--     spock_middleware $ wai_staticPolicy (wai_addBase "static")
--     spock_middleware wai_logStdoutDev
--     spock_get spock_root $ spock_file "test" "static/index.html"

--     spock_get "allpeople" $ do
--       (Ctx{ctxDB}) <- lift trans_get
--       js_json $ concatMap events (map_elems ctxDB)

--     spock_get "people" $ do
--       (Ctx{ctxDB}) <- lift trans_get
--       js_json
--         [ js_object
--           [ "value" .= ("All" :: TText)
--           , "open"  .= True
--           , "data"  .= map_elems ctxDB
--           ]
--         ]

--     spock_get ("hello2" <//> spock_var) $ \name -> do
--       (Ctx{ctxDB}) <- lift trans_get
--       spock_text $ t_pack . show $ map_findWithDefault "ok" ctxDB name -- ("Hello " <> name <> "!")

--     spock_get ("search" <//> spock_var) $ \ _name -> do
--       (Ctx{ctxConn}) <- lift trans_get
--       liftIO $ do

--         let ms1 = js_encode $ searchName _name
--         c8_putStrLn ms1
--         ws_sendTextData ctxConn ms1

--         ctrl_threadDelay 5000000

--         let ms2 = js_encode clickOnFirstResult
--         c8_putStrLn ms2
--         ws_sendTextData ctxConn ms2

--         ctrl_threadDelay 5000000

--         let ms3 = js_encode getExperiences
--         c8_putStrLn ms3
--         ws_sendTextData ctxConn ms3

--       spock_text _name
