module App where

import           ClassyPrelude
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Util                                 hiding (delete, get)
import           Web.Spock.Safe                       (get, text)

-- Handlers
import qualified Handler.Home                         as H
import qualified Handler.LinkShortener                as H
import qualified Handler.Register                     as H
import qualified Handler.Test                         as H
import qualified Handler.User                         as H
import           Hooks
import           Route

application :: IO Middleware
application = do
  config <- initSpockConfig
  spock config $ prehook baseHook $ do
    middleware logStdoutDev

    get     root      H.indexHandler
    getpost userR     H.userHandler
    get     "test"    H.testHandler
    getpost urlNewPath H.linkShortenerHandler
    get     urlPath   H.getUrl
    get     helloR    $ \name -> text $ "Hellso " <> name <> "!"
    get     addR      $ \a b -> text $ pack $ show (a + b)

    prehook guestOnlyHook $
      getpost registerR H.registerHandler

initSpockConfig :: IO Config
initSpockConfig = do
  pool <- runStdoutLoggingT $ createSqlitePool "funblog.db" 5
  runSqlPool (runMigration migrateCore) pool --
  return $ defaultSpockCfg Nothing (PCPool pool) (State 3)
