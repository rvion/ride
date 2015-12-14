
module Util
  ( module Util
  , module X
  ) where

-- Misc
-- import Control.Monad                as X (when, mplus, liftM)
-- import Control.Monad.IO.Class       as X (MonadIO, liftIO)
import           Control.Monad.Logger         as X
import           Control.Monad.Trans.Reader   as X (ReaderT)
import           Control.Monad.Trans.Resource as X (ResourceT, runResourceT)
import           Data.HVect                   as X (HVect (..), HVectElim,
                                                    HasRep, ListContains,
                                                    NotInList, findFirst)
-- import Data.Maybe                   as X (isJust)
-- import Data.Monoid                  as X ((<>))
import           Control.Exception            (SomeException (..))
import           Data.Pool                    as X (Pool, withResource)
-- import Data.String                  as X (fromString)
-- import Data.Text                    as X (Text, pack)
import           ClassyPrelude
import qualified Data.Text.Lazy               as LT
import           Data.Time                    as X

-- WEB
import           Web.Spock.Safe               as X hiding (SessionId, delete,
                                                    get, text)

-- View
import           Lucid                        as X
import           Template.Layout              as X (defaultLayout, simpleError)

-- Model
import           Database.Persist             as X
import           Database.Persist.Sql         as X
import           Database.Persist.Sqlite      as X
import           Models                       as X

import qualified Data.ByteString.Char8 as BS

page :: MonadIO m => Html a -> ActionCtxT ctx m b
page = lucid . defaultLayout

lucid :: MonadIO m => Html a -> ActionCtxT ctx m b
lucid = html . LT.toStrict . renderText

toS :: Show a => a -> HTML
toS = toHtml . show

asT :: Text -> HTML
asT = toHtml . unpack

-- Foundation Types
type Handler ctxBefore ctxAfter = SpockActionCtx ctxBefore DBConn Sess State ctxAfter
type Handler_ = Handler () ()
type DBPool = Pool DBConn
type DBConn = SqlBackend
type Config = SpockCfg DBConn Sess State
type Sess = Maybe SessionId
data State = State Int
type HTML = Html ()
type Req a = SqlPersistT (LoggingT (ResourceT IO)) a

asInt :: Int -> Int
asInt = id

-- type LoggerType =
-- logger = runNoLoggingT

db :: (SpockConn m ~ SqlBackend, HasSpock m) => Req a -> m a
db action = runQuery $ \conn -> runResourceT $ runStdoutLoggingT $ runSqlConn action conn
{-# INLINE db #-}

dbSafe :: (SpockConn m ~ SqlBackend, HasSpock m) => Req a -> m (Either Text a)
dbSafe action = runQuery $ \conn -> runResourceT $ runStdoutLoggingT $ catch
  (Right <$> runSqlConn action conn)
  (\(SomeException e) -> return . Left . tshow$ e )

-- db' cmd = runSqlite "funblog.db" cmd

unpackBS :: BS.ByteString -> String
unpackBS = BS.unpack
