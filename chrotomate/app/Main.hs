{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Chrome.Command
import           Chrome.DB
import           Chrome.Server
import           Opts
import JetPack

-- 2: Get the list of pages
data ChromiumPageInfo = ChromiumPageInfo
  { chromiumDebuggerUrl :: String
  , pageURL             :: String
  } deriving (Show)

instance FromJSON ChromiumPageInfo where
  parseJSON (JsObject obj) = ChromiumPageInfo
    <$> obj .: "webSocketDebuggerUrl"
    <*> obj .: "url"
  parseJSON _ = mzero

getChromiumPageInfo :: Int -> IO (Maybe [ChromiumPageInfo])
getChromiumPageInfo port = do
  request <- http_parseUrl ("http://localhost:" ++ show port ++ "/json")
  manager <- http_newManager http_tlsManagerSettings
  response <- http_httpLbs request manager
  return $ js_decode (get_http_responseBody response)

main :: IO ()
main = do
  Opts _dbpath <- opt_execParser fullopts
  hSetBuffering stdin NoBuffering

  _dbExists <- doesFileExist _dbpath
  _db <- if _dbExists then read <$> SIO.readFile _dbpath else return (map_fromList [])
  startTool _db _dbpath
  return ()
  where
    fullopts = opt_info (opt_helper <*> opts)
      (  opt_fullDesc
      <> opt_progDesc "Print a greeting for TARGET"
      <> opt_header "hello - a test for optparse-applicative" )

tryUntilItIsWorking :: String -> IO (Maybe a) -> IO a
tryUntilItIsWorking errMsg action =
  action >>= \mbres -> case mbres of
    Just a -> return a
    Nothing -> print errMsg >> ctrl_threadDelay 1000000 >> print "retrying.." >> tryUntilItIsWorking errMsg action

startTool :: DB -> FilePath -> IO ()
startTool _db _dbpath = do
  shared <- stm_atomically $ stm_newTVar 0
  pages <- tryUntilItIsWorking "please, close chrome debugger and press enter on this terminal" (getChromiumPageInfo 9160)
  let
    (host, port, path) = parseUri linkedinWS
    linkedinWS = chromiumDebuggerUrl linkedinPage
    linkedinPage = case filter (isPrefixOf "https://www.linkedin" . pageURL) pages of
      [] -> error "open linkedin page and login before running this program"
      (linkedinPage : _) -> linkedinPage

  ws_runClient host port path $ \_conn -> do
    let ctx = Ctx _db _dbpath _conn
    _ <- ctrl_forkIO (webserver ctx)
    loopAnalyse ctx

loopAnalyse :: Ctx -> IO ()
loopAnalyse ctx@(Ctx _db _dbpath _conn) = do
  msg <- ws_receiveData _conn
  putStrLn "-----" >> putStrLn msg >> putStrLn "-----"
  let mbres = js_decode msg :: Maybe CommandResult
      mbLifeS = mbres >>= \res -> Just (js_fromJSON (resultValue res))
      mbLife = mbLifeS >>= \lifeS -> case lifeS of {JsSuccess m -> m; JsError a -> traceShow a Nothing}

  print mbres

  case mbLife of
    Nothing ->
      loopAnalyse ctx
    Just life -> do
      putStrLn "------- LIFE --------"
      let newDB = map_insert (name life) life (ctxDB ctx)
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
  _u    <- uri_parseURI uri
  _auth <- uri_uriAuthority _u
  let _port = case Uri.uriPort _auth of (':' : _str) -> read _str; _ -> 80
  return (Uri.uriRegName _auth, _port, Uri.uriPath _u)


