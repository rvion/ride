{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where
import           Control.Monad              (forever, mzero)
import           Control.Monad.Trans        (liftIO)
import           Data.Aeson                 (FromJSON (..), (.:))

import           Chrome.DB
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (isPrefixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text.IO               as T
import           Debug.Trace
import           Options.Applicative
import           Opts
import           System.IO
import           System.IO.Strict           as SIO

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Network.HTTP.Conduit       as Http

import qualified Network.URI                as Uri
import qualified Network.WebSockets         as WS

import           Chrome.Command
import           Chrome.Server
-- import           System.Exit                (ExitCode)
import           Control.Concurrent
import           System.Directory           (doesFileExist)

import           Control.Concurrent.STM
import           System.Process             (system)


-- 2: Get the list of pages
data ChromiumPageInfo = ChromiumPageInfo
  { chromiumDebuggerUrl :: String
  , pageURL             :: String
  } deriving (Show)

instance FromJSON ChromiumPageInfo where
  parseJSON (A.Object obj) = ChromiumPageInfo
    <$> obj .: "webSocketDebuggerUrl"
    <*> obj .: "url"
  parseJSON _ = mzero

getChromiumPageInfo :: Int -> IO (Maybe [ChromiumPageInfo])
getChromiumPageInfo port = do
  request <- Http.parseUrl ("http://localhost:" ++ show port ++ "/json")
  manager <- Http.newManager Http.tlsManagerSettings
  response <- Http.httpLbs request manager
  return $ A.decode (Http.responseBody response)

main :: IO ()
main = do
  Opts _dbpath <- execParser fullopts
  hSetBuffering stdin NoBuffering

  _dbExists <- doesFileExist _dbpath
  _db <- if _dbExists then read <$> SIO.readFile _dbpath else return (M.fromList [])
  startTool _db _dbpath
  return ()
  where
    fullopts = info (helper <*> opts)
      ( fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )

tryUntilItIsWorking :: String -> IO (Maybe a) -> IO a
tryUntilItIsWorking errMsg action =
  action >>= \mbres -> case mbres of
    Just a -> return a
    Nothing -> print errMsg >> threadDelay 1000000 >> print "retrying.." >> tryUntilItIsWorking errMsg action

startTool :: DB -> FilePath -> IO ()
startTool _db _dbpath = do
  shared <- atomically $ newTVar 0
  pages <- tryUntilItIsWorking "please, close chrome debugger and press enter on this terminal" (getChromiumPageInfo 9160)
  let
    (host, port, path) = parseUri linkedinWS
    linkedinWS = chromiumDebuggerUrl linkedinPage
    linkedinPage = case filter (isPrefixOf "https://www.linkedin" . pageURL) pages of
      [] -> error "open linkedin page and login before running this program"
      (linkedinPage : _) -> linkedinPage

  WS.runClient host port path $ \_conn -> do
    let ctx = Ctx _db _dbpath _conn
    _ <- forkIO (webserver ctx)
    loopAnalyse ctx

loopAnalyse :: Ctx -> IO ()
loopAnalyse ctx@(Ctx _db _dbpath _conn) = do
  msg <- WS.receiveData _conn
  putStrLn "-----" >> C8.putStrLn msg >> putStrLn "-----"
  let mbres = A.decode msg :: Maybe CommandResult
      mbLifeS = mbres >>= \res -> Just (A.fromJSON (resultValue res))
      mbLife = mbLifeS >>= \lifeS -> case lifeS of {A.Success m -> m; A.Error a -> traceShow a Nothing}

  print mbres

  case mbLife of
    Nothing ->
      loopAnalyse ctx
    Just life -> do
      putStrLn "------- LIFE --------"
      let newDB = M.insert (name life) life (ctxDB ctx)
      let ctx' = ctx{ ctxDB = newDB }
      writeFile _dbpath (show newDB)
      print "written:"
      print newDB
      putStrLn "------------------"
      loopAnalyse ctx'
        -- txt <- getLine
        -- let cmd = searchName txt
        -- print (A.encode cmd)
        -- WS.sendTextData conn $ A.encode cmd

parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
  _u    <- Uri.parseURI uri
  _auth <- Uri.uriAuthority _u
  let _port = case Uri.uriPort _auth of (':' : _str) -> read _str; _ -> 80
  return (Uri.uriRegName _auth, _port, Uri.uriPath _u)


