{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Chrome.Server where

import           Jetpack
import           Chrome.Command
import           Chrome.DB
-- import Control.Monad.Trans.State (StateT)
data Ctx = Ctx
  { ctxDB     :: DB
  , ctxDbPath :: FilePath
  , ctxConn   :: WsConnection
  }
newtype M a = M { unM :: TransStateT Ctx IO a } deriving (Functor, Applicative, Monad, MonadIO)
-- newtype M a = M {unM :: TransStateT Ctx IO a} derving (Monad, MonadIO)

webserver ::Ctx -> IO ()
webserver _ctx = do
  port <- maybe 3000 read <$> env_lookupEnv "PORT"
  spock_runSpock port $ spock_spockT (runM _ctx) ride

runM :: Ctx -> M a -> IO a
runM _ctx m = trans_evalStateT (unM m) _ctx

-- a = spock_var
-- b = (<//>)

-- newtpe
-- ride = undefined
ride :: SpockSpockT M ()
ride = do
    spock_middleware $ wai_staticPolicy (wai_addBase "static")
    spock_middleware wai_logStdoutDev
    spock_get spock_root $ spock_file "test" "static/index.html"

    spock_get "allpeople" $ do
      let a  = lift
      (Ctx{ctxDB}) <- lift (M trans_get)
      spock_json $ concatMap events (map_elems ctxDB)

    spock_get "people" $ do
      (Ctx{ctxDB}) <- lift (M trans_get)
      spock_json
        [ js_object
          [ "value" .= ("All" :: TText)
          , "open"  .= True
          , "data"  .= map_elems ctxDB
          ]
        ]

    spock_get ("hello2" <//> spock_var) $ \(name :: String) -> do
      (Ctx{ctxDB}) <- lift $ M trans_get
      spock_text $ case map_lookup name ctxDB of
          Just a -> t_pack . show $ a -- ("Hello " <> name <> "!")
          Nothing -> "not found"

    spock_get ("search" <//> spock_var) $ \ _name -> do
      (Ctx{ctxConn}) <- lift $ M trans_get
      liftIO $ do

        let ms1 = js_encode $ searchName _name
        c8_putStrLn ms1
        ws_sendTextData ctxConn ms1

        ctrl_threadDelay 5000000

        let ms2 = js_encode clickOnFirstResult
        c8_putStrLn ms2
        ws_sendTextData ctxConn ms2

        ctrl_threadDelay 5000000

        let ms3 = js_encode getExperiences
        c8_putStrLn ms3
        ws_sendTextData ctxConn ms3

      spock_text _name
