{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Chrome.Server where

import JetPack
-- import           Control.Concurrent                   (forkIO, threadDelay)
-- import           Debug.Trace
-- import           Control.Monad.IO.Class               (liftIO)
-- import           Control.Monad.Trans.Class            (lift)
-- import           Control.Monad.Trans.State            (StateT, evalStateT)
-- import qualified Control.Monad.Trans.State            as S
-- import           System.Environment                   (lookupEnv)

-- import           Data.Monoid                          ((<>))

-- import           Chrome.Command
-- import qualified Data.Aeson                           as A
-- import qualified Data.ByteString.Lazy.Char8           as C8
-- import qualified Data.Map                             as M
-- import qualified Data.Text                            as T
-- import           Chrome.DB
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- import           Network.Wai.Middleware.Static        (addBase, staticPolicy)
-- import qualified Network.WebSockets                   as WS
-- import           Web.Spock

data Ctx = Ctx
  { ctxDB     :: DB
  , ctxDbPath :: FilePath
  , ctxConn   :: WSConnection
  }

type M = TransStateT Ctx IO

webserver ::Ctx -> IO ()
webserver _ctx = do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  spock_runSpock port $ spock_spockT (runM _ctx) ride

runM :: Ctx -> M a -> IO a
runM _ctx = flip trans_evalStateT _ctx

ride :: SpockSpockT M ()
ride = do
    spock_middleware $ wai_staticPolicy (wai_addBase "static")
    spock_middleware wai_logStdoutDev
    spock_get spock_root $ spock_file "test" "static/index.html"

    spock_get "allpeople" $ do
      (Ctx{ctxDB}) <- trans_lift trans_get
      js_json $ concatMap events (map_elems ctxDB)

    spock_get "people" $ do
      (Ctx{ctxDB}) <- trans_lift trans_get
      js_json [ js_object
          [ "value" .= ("All" :: TText)
          , "open" .= True
          , "data"  .= map_elems ctxDB
          ]
        ]



    spock_get ("hello2" <//> spock_var) $ \name -> do
      (Ctx{ctxDB}) <- trans_lift trans_get
      spock_text $ t_pack . show $ ctxDB M.! name -- ("Hello " <> name <> "!")

    spock_get ("search" <//> spock_var) $ \ _name -> do
      (Ctx{ctxConn}) <- trans_lift trans_get
      trans_liftIO $ do

        let ms1 = js_encode $ searchName _name
        c8_putStrLn ms1
        ws_sendTextData ctxConn ms1

        threadDelay 5000000

        let ms2 = js_encode clickOnFirstResult
        c8_putStrLn ms2
        ws_sendTextData ctxConn ms2

        threadDelay 5000000

        let ms3 = js_encode getExperiences
        c8_putStrLn ms3
        ws_sendTextData ctxConn ms3

      spock_text _name
