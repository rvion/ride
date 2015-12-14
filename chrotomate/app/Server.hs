{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Server where

import           Control.Concurrent                   (threadDelay, forkIO)
import           Debug.Trace
-- import           Control.Lens                         ()
-- import           Control.Monad                        ()
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.State            (StateT, evalStateT)
import qualified Control.Monad.Trans.State            as S
import System.Environment (lookupEnv)

-- import           Data.Aeson                           (Value, object, (.=))
import           Data.Monoid                          ((<>))

-- import           Data.Map                             (Map)
-- import qualified Data.Map                             as M
-- import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Network.WebSockets         as WS
import qualified Data.Map as M
import           DB
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, staticPolicy)
import           Web.Spock
import qualified Data.Aeson as A
import Command

data Ctx = Ctx
  { ctxDB   :: DB
  , ctxDbPath :: FilePath
  , ctxConn :: WS.Connection
  }

type M = StateT Ctx IO

webserver ::Ctx -> IO ()
webserver _ctx = do
  port <- maybe 3000 read <$> (lookupEnv "PORT")
  runSpock port $ spockT (runM _ctx) ride

runM :: Ctx -> M a -> IO a
runM _ctx = flip evalStateT _ctx

ride :: SpockT M ()
ride = do
    middleware $ staticPolicy (addBase "static")
    middleware logStdoutDev
    get root $ file "test" "static/index.html"

    get "allpeople" $ do
      (Ctx{ctxDB}) <- lift S.get
      json $ concatMap events (M.elems ctxDB)

    get "people" $ do
      (Ctx{ctxDB}) <- lift S.get
      json $ [ A.object
          [ "value" A..= ("All" :: T.Text)
          , "open" A..= True
          , "data"  A..= (M.elems ctxDB)
          ]
        ]



    get ("hello2" <//> var) $ \name -> do
      (Ctx{ctxDB}) <- lift S.get
      text $ T.pack . show $ ctxDB M.! name -- ("Hello " <> name <> "!")

    get ("search" <//> var) $ \ _name -> do
      (Ctx{ctxConn}) <- lift S.get
      liftIO $ do

        let ms1 = A.encode $ searchName _name
        C8.putStrLn ms1
        WS.sendTextData ctxConn ms1

        threadDelay 5000000

        let ms2 = A.encode clickOnFirstResult
        C8.putStrLn ms2
        WS.sendTextData ctxConn ms2

        threadDelay 5000000

        let ms3 = A.encode getExperiences
        C8.putStrLn ms3
        WS.sendTextData ctxConn ms3

      text (_name)
